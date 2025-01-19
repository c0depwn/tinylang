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

/*
dbg: .asciz "ok\n"
mv s2, x10
la a0, dbg
jal ra, printf
mv a0, s2

*/

//func (libcGenerator) provideBuiltInCopy() builtInFunc {
//	// copy(dst, src, n), works for arrays and strings
//	// _copy_builtin(dst, src, size)
//	// libc memcopy(dst, src, size)
//
//	return builtInFunc{
//		name: "copy",
//		rewriteCall: func(expr *ast.CallExpression) {
//			// TODO: const arch32 not good
//
//			// copied to dst
//			dstArg := expr.Arguments[0]
//			sizeOfDst := sizeOf(dstArg.Type().Underlying(), architectureRV32)
//
//			// copied from src
//			srcArg := expr.Arguments[1]
//			sizeOfSrc := sizeOf(srcArg.Type().Underlying(), architectureRV32)
//
//			// ensure compatible, works because we have enough info at comp-time
//			if sizeOfDst < sizeOfSrc {
//				panic(fmt.Sprintf("copy: size of dst must be larger than or equal to size of src: must be at least %d bytes", sizeOfDst))
//			}
//
//			// call libc directly as the signatures match
//			expr.Function.Name = "memcpy"
//		},
//		generate: func(g *Generator) {},
//	}
//}

//func (libcGenerator) provideBuiltNullTerminate() builtInFunc {
//	// convert a TinyLang string to a C string
//	// - remove length of string at position 0 in memory
//	// - null terminate at the end of string
//	// - return a pointer to the converted string
//
//	// str:
//	// +--------------------+
//	// | len | characters...|
//	// +--------------------+
//
//	// _builtin_cstring(str):
//	// +-------------------+
//	// | characters...| \0 |
//	// +-------------------+
//
//	// _builtin_cstring(str) -> str
//
//	return builtInFunc{
//		name:        "_builtin_cstring",
//		rewriteCall: nil, // cannot be called directly
//		generate: func(g *Generator) {
//			g.asm.EmitRawProcedure("_builtin_cstring", `
//# prologue
//addi sp, sp, -16 # allocate 16 bytes on the stack
//sw   ra, 12(sp)  # save return address
//sw   s0, 8(sp)   # save frame pointer
//addi s0, sp, 16  # set frame pointer
//
//# parameter a0: contains the pointer to the string
//
//# save the pointer to the source string into s1
//mv s1, a0
//
//# save the num of chars of the string into s2
//lw   t0, 0(s1)
//addi s2, t0, 1
//
//# load length of string
//lw  t0, 0(s1)  # t0 now contains the number of chars of the original string
//
//# allocate len(string)+1 memory using malloc
//addi a0, t0, 1   # a0 now contains the length needed to allocate the new string
//jal ra, malloc   # call malloc
//add s2, x0, a0   # save returned pointer into s2
//
//# copy the original string using memcpy
//mv
//# a0: already contains destination address from malloc call
//lw a0, s2        # src
//lw a1, s1        # dst
//lw a2, source_len            # a2 = number of bytes to copy (13)
//
//
//lw   ra, 12(sp)  # restore return address
//lw   s0, 8(sp)   # restore frame pointer
//addi sp, sp, 16  # deallocate stack space
//ret
//`)
//		},
//	}
//}

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
