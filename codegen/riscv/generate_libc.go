package riscv

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/types"
)

type libcGenerator struct{}

// TODO: improvement: provide manual memory management capabilities when linking with libc
// TODO: improvement: provide interoperability with C strings (null terminated)
// TODO: improvement: provide built-in copy(dst, src)
// TODO: improvement: provide built-in append
// TODO: improvement: provide built-in memcopy(dst, src, n)
// TODO: improvement: T(X) for type casting/conversion
func (gen libcGenerator) exec(g *Generator, f *ast.File) (string, error) {
	// compile internal functions, non-callable by user
	gen.genMalloc(g)
	genInit(g, f)

	// provide callable builtins
	registerAndGenerateBuiltIn(
		g,
		gen.provideBuiltInPrint(),
		gen.provideBuiltInPrintLn(),
		provideBuiltInLen(),
	)

	// pre-pend a call to _internal_init in main
	prependInternalInit(f)

	// make sure linker sees main as a global symbol
	g.asm.AddDirective(".globl main")

	return g.exec(f)
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

func (libcGenerator) genMalloc(g *Generator) {
	// libc initializes the heap itself, we can simply provide a malloc wrapper
	g.asm.BeginProcedure(internalMalloc)
	emitDefaultPrologue(g)
	g.asm.Call("malloc")
	emitDefaultEpilogue(g)
}

func (libcGenerator) provideBuiltInPrint() builtInFunc {
	// print(T)
	// if T = string, print(string) => printf(null_terminated(string))
	// if T = int,    print(int)    => printf("%d", int)
	// if T = bool,   print(bool)   => printf("%d", bool)
	// if T = array,  unsupported

	const internalBuiltInPrintInt = "_builtin_wrap_printf_i"
	// alternative: transform print(arg) to printf(fmt_string, arg)
	return builtInFunc{
		name: "print",
		rewriteCall: func(expr *ast.CallExpression) {
			switch expr.Arguments[0].Type().Underlying().(type) {
			case types.Bool:
				expr.Function.Name = internalBuiltInPrintInt
			case types.Int:
				expr.Function.Name = internalBuiltInPrintInt
			default:
				panic(fmt.Sprintf("unsupported type for libc printf call: %s", expr))
			}
		},
		generate: func(g *Generator) {
			// pre-defined zero-terminated format strings for supported types
			g.asm.DefineData("_print_fmt_i", emitData{kind: ".asciz", value: "\"%d\""})

			g.asm.BeginProcedure(internalBuiltInPrintInt)
			emitDefaultPrologue(g)
			g.asm.Move(a1, a0)
			g.asm.LoadDataAddress("_print_fmt_i", a0)
			g.asm.Call("printf")
			emitDefaultEpilogue(g)
		},
	}
}

func (libcGenerator) provideBuiltInPrintLn() builtInFunc {
	// print(T)
	// if T = string, print(string) => printf(null_terminated(string))
	// if T = int,    print(int)    => printf("%d", int)
	// if T = bool,   print(bool)   => printf("%d", bool)
	// if T = array,  unsupported

	const internalBuiltInPrintLnInt = "_builtin_wrap_println_i"

	return builtInFunc{
		name: "println",
		rewriteCall: func(expr *ast.CallExpression) {
			switch expr.Arguments[0].Type().Underlying().(type) {
			case types.Bool:
				expr.Function.Name = internalBuiltInPrintLnInt
			case types.Int:
				expr.Function.Name = internalBuiltInPrintLnInt
			default:
				panic(fmt.Sprintf("unsupported type for libc printf call: %s", expr))
			}
		},
		generate: func(g *Generator) {
			// pre-defined zero-terminated format strings for supported types
			g.asm.DefineData("_println_fmt_i", emitData{kind: ".asciz", value: "\"%d\\n\""})

			g.asm.BeginProcedure(internalBuiltInPrintLnInt)
			emitDefaultPrologue(g)
			g.asm.Move(a1, a0)
			g.asm.LoadDataAddress("_println_fmt_i", a0)
			g.asm.Call("printf")
			emitDefaultEpilogue(g)
		},
	}
}
