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
	internalInit   = "_internal_init"
	internalMalloc = "_internal_malloc"
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
						T: types.Function{
							Params: nil,
							Result: types.NewVoid(),
						},
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
				resultReg := g.expression(init.Expression)
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
