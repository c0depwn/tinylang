package riscv

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
)

type ripesGenerator struct{}

// TODO: improvement: provide built-in read(src, n)
// TODO: improvement: provide manual memory management capabilities when linking with libc
// TODO: improvement: provide interoperability with C strings (null terminated)
// TODO: improvement: provide built-in copy(dst, src)
// TODO: improvement: provide built-in append
// TODO: improvement: provide built-in memcopy(dst, src, n)
// TODO: improvement: T(X) for type casting/conversion
func (gen ripesGenerator) exec(g *Generator, f *ast.File) (string, error) {
	// compile internal functions, non-callable by user
	gen.genStart(g.asm)
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
		gen.provideBuiltInRead(),
		provideBuiltInLen(),
		provideBuiltInConvertToByte(),
		provideBuiltInConvertToInt(),
		provideBuiltInConvertToString(),
	)

	// pre-pend a call to _internal_init in main
	prependInternalInit(f)

	return g.exec(f)
}

// genStart generates the entry point procedure.
// This procedure is called initially when running a program.
// It calls the main function defined by the programmer
// and invokes exist when main completes.
// Start cannot be used or invoked by programmers directly.
func (ripesGenerator) genStart(asm pseudoASM) {
	asm.EmitRawProcedure("_start", `
jal ra, main   # invoke main
li a7, 10      # exit syscall
ecall
ret
`)
}

// genMalloc adds a naive bump allocator.
// The supplied size will be allocated on the heap and a pointer to
// the allocated memory is returned. The heap starts at a fixed address.
// There is no "free" implementation, the heap will grow indefinitely.
// No checks are implemented to avoid writing into the stack.
// The resulting procedure is available at the label internalMalloc.
// Arguments: 	size, passed through a0
// Returns: 	pointer, returned through a0
func (ripesGenerator) genMalloc(g *Generator) {
	// points at the bottom (start) of the heap
	g.asm.DefineData("heap_base", emitInt(g.platform, 0x1000))
	// point at the next available address
	g.asm.DefineData("heap_ptr", emitInt(g.platform, 0x1000))

	g.asm.BeginProcedure(internalMalloc)
	emitDefaultPrologue(g)

	// t0: addr to heap_ptr
	// t1: value of heap_ptr
	// t2: new heap_ptr

	// read heap_base value and store it in t0
	g.asm.LoadDataAddress("heap_ptr", t0)
	g.asm.LoadFromOffset(t1, t0, 0)

	// calculate new heap_ptr: current + requested space
	g.asm.Add(t2, t1, a0)

	// store current heap pointer as return value in a0
	g.asm.Move(a0, t1)

	// save the updated heap_ptr
	g.asm.StoreAtOffset(t2, t0, 0)

	emitDefaultEpilogue(g)
}

func (gen ripesGenerator) provideBuiltInPrint() builtInFunc {
	return builtInFunc{
		name: "print",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = fmt.Sprintf("_print_%s", expr.Arguments[0].Type().Underlying())
		},
		generate: func(g *Generator) {
			g.asm.BeginProcedure("_print_int")
			emitDefaultPrologue(g)
			g.asm.LoadImmediate(a7, 1)
			g.asm.SysCall()
			emitDefaultEpilogue(g)

			g.asm.BeginProcedure("_print_byte")
			emitDefaultPrologue(g)
			g.asm.LoadImmediate(a7, 1)
			g.asm.SysCall()
			emitDefaultEpilogue(g)

			g.asm.BeginProcedure("_print_bool")
			emitDefaultPrologue(g)
			g.asm.LoadImmediate(a7, 1)
			g.asm.SysCall()
			emitDefaultEpilogue(g)

			// TODO: bug: won't work 100% because ripes expects null terminated string
			g.asm.BeginProcedure("_print_string")
			emitDefaultPrologue(g)
			g.asm.Call(internalCString)
			g.asm.LoadImmediate(a7, 4)
			g.asm.SysCall()
			emitDefaultEpilogue(g)
		},
	}
}

func (gen ripesGenerator) provideBuiltInPrintLn() builtInFunc {
	return builtInFunc{
		name: "println",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = fmt.Sprintf("_println_%s", expr.Arguments[0].Type().Underlying())
		},
		generate: func(g *Generator) {
			// print(int) then print("\n")
			g.asm.BeginProcedure("_println_int")
			emitDefaultPrologue(g)
			g.asm.Call("_print_int")
			g.asm.LoadImmediate(a0, 10)
			g.asm.LoadImmediate(a7, 11)
			g.asm.SysCall()
			emitDefaultEpilogue(g)

			g.asm.BeginProcedure("_println_byte")
			emitDefaultPrologue(g)
			g.asm.Call("_print_byte")
			g.asm.LoadImmediate(a0, 10)
			g.asm.LoadImmediate(a7, 11)
			g.asm.SysCall()
			emitDefaultEpilogue(g)

			// print(int) then print("\n")
			g.asm.BeginProcedure("_println_bool")
			emitDefaultPrologue(g)
			g.asm.Call("_print_int")
			g.asm.LoadImmediate(a0, 10)
			g.asm.LoadImmediate(a7, 11)
			g.asm.SysCall()
			emitDefaultEpilogue(g)

			// print(str) then print("\n")
			g.asm.BeginProcedure("_println_string")
			emitDefaultPrologue(g)
			g.asm.Call("_print_string")
			g.asm.LoadImmediate(a0, 10)
			g.asm.LoadImmediate(a7, 11)
			g.asm.SysCall()
			emitDefaultEpilogue(g)
		},
	}
}

func (gen ripesGenerator) provideBuiltInRead() builtInFunc {
	// read(fd, dst, n) -> int
	const internalRead = "_internal_read"
	return builtInFunc{
		name: "read",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = internalRead
		},
		generate: func(g *Generator) {
			g.asm.BeginProcedure(internalRead)

			emitDefaultPrologue(g)
			defer emitDefaultEpilogue(g)

			// a0 = fd
			// a1 = buf
			g.asm.AddImmediate(a1, a1, g.platform.RegisterSize())
			// a2 = len
			// syscall read(fd, dst, n)
			g.asm.LoadImmediate(a7, 63)
			g.asm.SysCall()
		},
	}
}
