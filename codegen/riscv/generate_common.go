package riscv

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/initialization"
	"github.com/c0depwn/tinylang/types"
	"slices"
)

// builtInGenerator abstracts Target specific generation details.
type builtInGenerator interface {
	exec(g *Generator, f *ast.File) (string, error)
}

type Target string

const (
	TargetRipes Target = "ripes"
	TargetLibC  Target = "libc"
)

func fromTarget(t Target) builtInGenerator {
	var g builtInGenerator
	switch t {
	case TargetRipes:
		return ripesGenerator{}
	case TargetLibC:
		return libcGenerator{}
	default:
		panic(fmt.Sprintf("invalid target: %s", t))
	}
	return g
}

const (
	internalInit     = "_internal_init"
	internalMalloc   = "_internal_malloc"
	internalCString  = "_internal_cstring"
	internalTString  = "_internal_tstring"
	internalToByte   = "_internal_to_byte"
	internalToInt    = "_internal_to_int"
	internalToString = "_internal_to_string"

	builtinRead = "_builtin_read"
)

func prependInternalInit(f *ast.File) {
	// prepend the init call inside main
	for _, declaration := range f.Declarations {
		if funcDecl, ok := declaration.(*ast.FuncDeclaration); ok {
			if funcDecl.Name() != "main" {
				continue
			}

			funcDecl.Body.Statements = append([]ast.Statement{
				&ast.ExpressionStatement{
					Expression: &ast.CallExpression{
						Function:  &ast.Identifier{Name: internalInit},
						Arguments: []ast.Expression{},
						T:         types.NewVoid(),
					},
				},
			}, funcDecl.Body.Statements...)
		}
	}
}

// genInit generates a procedure which initializes global
// declarations such as variables and constants.
// The init function is called before main and cannot
// be used by programmers directly
func genInit(g *Generator, file *ast.File) {
	// extract initialize-able declarations
	inits := []initialization.Initable{}
	for _, decl := range file.Declarations {
		initDecl, ok := decl.(initialization.Initable)
		if !ok {
			continue
		}
		inits = append(inits, initDecl)
	}

	// sort according to order
	slices.SortStableFunc(inits, func(a, b initialization.Initable) int {
		return a.GetInitOrder() - b.GetInitOrder()
	})

	g.asm.BeginProcedure(internalInit)

	// func prologue
	g.asm.AddImmediate(sp, sp, -16)
	g.asm.StoreAtOffset(ra, sp, 16-g.platform.RegisterSize())
	g.asm.StoreAtOffset(s0, sp, 16-g.platform.RegisterSize()*2)
	g.asm.AddImmediate(s0, sp, 16)

	// code-gen init
	for _, init := range inits {
		switch init := init.(type) {

		// init var
		case *ast.VarDeclaration:

			// define in data section
			g.asm.DefineData(init.Name(), emitZero(g.platform, init))

			if init.Expression != nil {
				// TODO: bug: for arrays heap allocations are required and currently don't work.

				// evaluate expression
				resultReg, _ := g.expression(init.Expression)
				if types.IsPointer(init.Expression.Type()) {
					g.asm.LoadFromOffset(resultReg, resultReg, 0)
				}

				// find address to store to & write to address
				storeAddrReg := g.registers.allocTemp()
				g.asm.LoadDataAddress(init.Name(), storeAddrReg)
				g.asm.StoreAtOffset(resultReg, storeAddrReg, 0)

				// free registers
				g.registers.free(storeAddrReg)
				g.registers.free(resultReg)
			}

		// init const
		case *ast.ConstDeclaration:
			// define in data section
			g.asm.DefineData(init.Name(), emitConst(g.platform, init.Expression))
		}
	}

	// func epilogue
	g.asm.LoadFromOffset(s0, sp, 16-g.platform.RegisterSize()*2)
	g.asm.LoadFromOffset(ra, sp, 16-g.platform.RegisterSize())
	g.asm.AddImmediate(sp, sp, 16)
	g.asm.Return()
}

// genInternalCString generates a procedure which converts
// a length-prefixed string into a null terminated string.
// The procedure returns the pointer to the newly created
// string.
func genInternalCString(g *Generator) {
	g.asm.BeginProcedure(internalCString)

	emitDefaultPrologue(g)
	defer emitDefaultEpilogue(g)

	// a0 = ptr to tl string (points to length field)

	// s1 = num of characters of src string
	g.asm.LoadFromOffset(s1, a0, 0)
	// s2 = ptr to src string
	g.asm.Move(s2, a0)

	// t0 = malloc(len(src) + 1)
	g.asm.Move(a0, s1)
	g.asm.AddImmediate(a0, a0, 1)
	g.asm.Call(internalMalloc)
	g.asm.Move(t0, a0)

	// make t1 point to first char of src
	g.asm.AddImmediate(t1, s2, g.platform.RegisterSize())
	// make t2 point to the last char of src
	g.asm.Add(t2, t1, s1)

	// while src addr <= max src addr
	g.asm.Insert("_internal_cstring_loop_start")
	g.asm.LessThan(t4, t1, t2)
	g.asm.BranchEQZ("_internal_cstring_loop_end", t4)

	// copy src[idx] to dst[idx] via t3 register and inc address
	g.asm.LoadNFromOffset(sizeByte, t3, t1, 0)
	g.asm.StoreNAtOffset(sizeByte, t3, t0, 0)
	g.asm.AddImmediate(t1, t1, 1)
	g.asm.AddImmediate(t0, t0, 1)
	g.asm.Jump("_internal_cstring_loop_start")
	g.asm.Insert("_internal_cstring_loop_end")

	// add null terminator in dst string
	g.asm.LoadImmediate(t3, 0)
	g.asm.StoreNAtOffset(sizeByte, t3, t0, 0)
}

// genInternalTString generates a procedure which converts
// a null-terminated string to a length-prefixed string.
// The procedure returns the pointer to the newly created
// string.
func genInternalTString(g *Generator) {
	g.asm.BeginProcedure(internalTString)

	emitDefaultPrologue(g)
	defer emitDefaultEpilogue(g)

	// s1 = pointer to str
	g.asm.Move(s1, a0)
	// s2 = length of string
	g.asm.AddImmediate(s2, x0, 0)

	// iterate until \0 to find length
	g.asm.Move(t0, a0)
	g.asm.Insert("_internal_tstring_len_loop_start")
	g.asm.LoadNFromOffset(sizeByte, t1, t0, 0)
	g.asm.BranchEQZ("_internal_tstring_len_loop_end", t1)
	g.asm.AddImmediate(t0, t0, 1)
	g.asm.AddImmediate(s2, s2, 1)
	g.asm.Jump("_internal_tstring_len_loop_start")
	g.asm.Insert("_internal_tstring_len_loop_end")

	// alloc length of src string + size of int
	g.asm.LoadImmediate(a0, g.platform.RegisterSize())
	g.asm.Add(a0, a0, t1)
	g.asm.Call(internalMalloc)

	// t2 = pointer to new string
	g.asm.Move(t2, a0)
	// save length field and increment address
	g.asm.StoreAtOffset(s2, t2, 0)
	g.asm.AddImmediate(t2, t2, g.platform.RegisterSize())

	g.asm.Move(t0, s1)
	g.asm.Insert("_internal_tstring_cpy_loop_start")
	g.asm.LoadNFromOffset(sizeByte, t1, t0, 0)
	g.asm.BranchEQZ("_internal_tstring_cpy_loop_end", t1)
	g.asm.LoadNFromOffset(sizeByte, t3, t0, 0)
	g.asm.StoreNAtOffset(sizeByte, t3, t2, 0)
	g.asm.AddImmediate(t0, t0, 1)
	g.asm.AddImmediate(t2, t2, 1)
	g.asm.Jump("_internal_tstring_cpy_loop_start")
	g.asm.Insert("_internal_tstring_cpy_loop_end")
}

func emitDefaultPrologue(g *Generator) {
	// allocate space on the stack
	g.asm.AddImmediate(sp, sp, -16)
	// save the return address
	g.asm.StoreAtOffset(ra, sp, 16-g.platform.RegisterSize())
	// save the frame pointer
	g.asm.StoreAtOffset(s0, sp, 16-g.platform.RegisterSize()*2)
	// set the frame pointer to the base of the stack frame
	g.asm.AddImmediate(s0, sp, 16)
}

func emitDefaultEpilogue(g *Generator) {
	// restore the frame pointer
	g.asm.LoadFromOffset(s0, sp, 16-g.platform.RegisterSize()*2)
	// restore the return address
	g.asm.LoadFromOffset(ra, sp, 16-g.platform.RegisterSize())
	// de-allocate stack frame
	g.asm.AddImmediate(sp, sp, 16)
	// return to caller
	g.asm.Return()
}
