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

	// string utils
	genInternalCString(g)
	genInternalTString(g)

	// provide callable builtins
	registerAndGenerateBuiltIn(
		g,
		gen.provideBuiltInPrint(),
		gen.provideBuiltInPrintLn(),
		provideBuiltInLen(),
		provideBuiltInConvertToByte(),
		provideBuiltInConvertToInt(),
		provideBuiltInConvertToString(),
		gen.provideBuiltInRead(),
	)

	// pre-pend a call to _internal_init in main
	prependInternalInit(f)

	// make sure linker sees main as a global symbol
	g.asm.AddDirective(".globl main")

	return g.exec(f)
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

	const (
		internalBuiltInPrintInt = "_builtin_wrap_printf_i"
		internalBuiltInPrintStr = "_builtin_wrap_printf_str"
	)
	// alternative: transform print(arg) to printf(fmt_string, arg)
	return builtInFunc{
		name: "print",
		rewriteCall: func(expr *ast.CallExpression) {
			switch expr.Arguments[0].Type().Underlying().(type) {
			case types.Bool:
				expr.Function.Name = internalBuiltInPrintInt
			case types.Byte:
				expr.Function.Name = internalBuiltInPrintInt
			case types.String:
				expr.Function.Name = internalBuiltInPrintStr
			case types.Int:
				expr.Function.Name = internalBuiltInPrintInt
			default:
				panic(fmt.Sprintf("unsupported type for libc printf call: %s", expr))
			}
		},
		generate: func(g *Generator) {
			// pre-defined zero-terminated format strings for supported types
			g.asm.DefineData("_print_fmt_i", emitData{kind: ".asciz", value: "\"%d\""})
			g.asm.DefineData("_print_fmt_s", emitData{kind: ".asciz", value: "\"%s\""})

			g.asm.BeginProcedure(internalBuiltInPrintInt)
			emitDefaultPrologue(g)
			g.asm.Move(a1, a0)
			g.asm.LoadDataAddress("_print_fmt_i", a0)
			g.asm.Call("printf")
			emitDefaultEpilogue(g)

			g.asm.BeginProcedure(internalBuiltInPrintStr)
			emitDefaultPrologue(g)
			g.asm.Call(internalCString)
			g.asm.Move(a1, a0)
			g.asm.LoadDataAddress("_print_fmt_s", a0)
			g.asm.Call("printf")
			emitDefaultEpilogue(g)
		},
	}
}

func (libcGenerator) provideBuiltInPrintLn() builtInFunc {
	// print(T)
	// if T = string, print(string) => printf(null_terminated(string))
	// if T = int,    print(int)    => printf("%d", int)
	// if T = byte,   print(int)    => printf("%d", byte)
	// if T = bool,   print(bool)   => printf("%d", bool)
	// if T = bool,   print(string) => printf("%s", string)
	// if T = array,  unsupported

	const (
		internalBuiltInPrintLnInt = "_builtin_wrap_println_i"
		internalBuiltInPrintLnStr = "_builtin_wrap_println_str"
	)

	return builtInFunc{
		name: "println",
		rewriteCall: func(expr *ast.CallExpression) {
			switch expr.Arguments[0].Type().Underlying().(type) {
			case types.Bool:
				expr.Function.Name = internalBuiltInPrintLnInt
			case types.Int:
				expr.Function.Name = internalBuiltInPrintLnInt
			case types.Byte:
				expr.Function.Name = internalBuiltInPrintLnInt
			case types.String:
				expr.Function.Name = internalBuiltInPrintLnStr
			default:
				panic(fmt.Sprintf("unsupported type for libc printf call: %s", expr))
			}
		},
		generate: func(g *Generator) {
			// pre-defined zero-terminated format strings for supported types
			g.asm.DefineData("_println_fmt_i", emitData{kind: ".asciz", value: "\"%d\\n\""})
			g.asm.DefineData("_println_fmt_s", emitData{kind: ".asciz", value: "\"%s\\n\""})

			g.asm.BeginProcedure(internalBuiltInPrintLnInt)
			emitDefaultPrologue(g)
			g.asm.Move(a1, a0)
			g.asm.LoadDataAddress("_println_fmt_i", a0)
			g.asm.Call("printf")
			emitDefaultEpilogue(g)

			g.asm.BeginProcedure(internalBuiltInPrintLnStr)
			emitDefaultPrologue(g)
			g.asm.Call(internalCString)
			g.asm.Move(a1, a0)
			g.asm.LoadDataAddress("_println_fmt_s", a0)
			g.asm.Call("printf")
			emitDefaultEpilogue(g)
		},
	}
}

func (libcGenerator) provideBuiltInRead() builtInFunc {
	// read(fd, dst, n) -> int
	return builtInFunc{
		name: "read",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = builtinRead
		},
		generate: func(g *Generator) {
			g.asm.BeginProcedure(builtinRead)

			emitDefaultPrologue(g)
			defer emitDefaultEpilogue(g)

			// a0 = fd
			// a1 = buf
			g.asm.AddImmediate(a1, a1, g.platform.RegisterSize())
			// a2 = len
			// libc call read(fd, dst, n)
			g.asm.Call("read")
		},
	}
}
