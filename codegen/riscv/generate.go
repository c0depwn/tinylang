package riscv

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	"github.com/c0depwn/tinylang/initialization"
	"github.com/c0depwn/tinylang/semantics"
	"github.com/c0depwn/tinylang/symbols"
	"github.com/c0depwn/tinylang/types"
	"io"
)

type GeneratorOptions func(*Generator)

func WithTarget(target Target) GeneratorOptions {
	return func(g *Generator) {
		g.target = target
	}
}

func WithPlatform(platform PlatformID) GeneratorOptions {
	return func(g *Generator) {
		switch platform {
		case PlatformRV32:
			g.platform = RV32Platform{}
			g.asm = newPseudoASM32Impl()
		case PlatformRV64:
			g.platform = RV64Platform{}
			g.asm = newPseudoASM64Impl()
		default:
			panic(fmt.Sprintf("unknown platform %v", platform))
		}
	}
}

func Generate(file *ast.File, out io.Writer, opts ...GeneratorOptions) error {
	// execute required AST passes before compilation
	// symbols -> types -> semantics -> initialization
	symbolInfo, err := symbols.Analyze(file)
	if err != nil {
		return err
	}
	if err := types.Analyze(file, symbolInfo); err != nil {
		return err
	}
	if err := semantics.Analyze(file, symbolInfo); err != nil {
		return err
	}
	if err := initialization.Analyze(file, symbolInfo); err != nil {
		return err
	}

	// finally, perform code generation

	g := &Generator{
		target: TargetRipes,

		// architecture specific
		asm:      newPseudoASM32Impl(),
		platform: RV32Platform{},

		env: &environment{},

		symbols: symbolInfo,
		//scope:       symbolInfo.DefinedBy(file),
		declFuncCtx: nil,
		registers:   newRegisterManager(),
	}

	for _, opt := range opts {
		opt(g)
	}

	g.symbols.Before(file)
	defer g.symbols.After(file)

	specificGen := fromTarget(g.target)
	pseudoasm, err := specificGen.exec(g, file)
	if err != nil {
		return err
	}

	_, err = io.WriteString(out, pseudoasm)
	return err
}

type Generator struct {
	asm      pseudoASM
	target   Target
	platform Platform

	symbols symbols.Info

	env *environment

	// scope is the current symbols.Scope the
	// generator is traversing
	scope *symbols.Scope

	// current function the generator is in
	declFuncCtx *ast.FuncDeclaration

	registers *RegisterManager
}

func (g *Generator) exec(file *ast.File) (string, error) {
	for _, declaration := range file.Declarations {
		switch d := declaration.(type) {
		case *ast.FuncDeclaration:
			// do not compile built-in functions here
			if _, exists := findBuiltIn(d.Name()); exists {
				panic(fmt.Sprintf("shadowed built-in function %s", d.Name()))
			}

			g.symbols.Before(d)
			g.fnDeclaration(d)
			g.symbols.After(d)
		}
	}

	return g.asm.String(), nil
}

func (g *Generator) statement(statement ast.Statement) {
	switch s := statement.(type) {
	case *ast.VarDeclaration:
		g.varDeclaration(s)
	case *ast.ConstDeclaration:
		g.constDeclaration(s)
	case *ast.Assignment:
		g.assignment(s)
	case *ast.Block:
		g.block(s)
	case *ast.IfStatement:
		g.ifStatement(s)
	case *ast.WhileStatement:
		g.whileStatement(s)
	case *ast.ReturnStatement:
		g.returnStatement(s)
	case *ast.ExpressionStatement:
		// expression statements simply ignore the result of the expression
		resultReg, _ := g.expression(s.Expression)
		g.registers.free(resultReg)
	default:
		panic(fmt.Errorf("unsupported statement type: %T", statement))
	}
}

func (g *Generator) block(block *ast.Block) {
	g.symbols.Before(block)
	defer g.symbols.After(block)
	//g.scope = g.symbols.DefinedBy(block)

	for _, statement := range block.Statements {
		g.statement(statement)
	}
}

// varDeclaration generates code for a variable declaration and
// its initialization expression (if it exists).
//
// Preconditions: The environment must have the stack offset registered for the variable's identifier.
func (g *Generator) varDeclaration(declaration *ast.VarDeclaration) {
	// If there is no initialisation, implicitly set the value to 0 using the x0 register,
	// This ensures that there is always a valid 'zero value' for uninitialized vars.
	var resultRegister Register
	var sizeOfResult size

	// compile the expression if there is one
	if declaration.Expression != nil {
		resultRegister, sizeOfResult = g.expression(declaration.Expression)

		// dereference pointer (e.g. *T -> T)
		if types.IsPointer(declaration.Expression.Type()) {
			// g.asm.DebugAddComment("deref pointer result")
			g.asm.LoadFromOffset(resultRegister, resultRegister, 0)
		}

		defer g.registers.free(resultRegister)
	} else {

		resultRegister = g.registers.allocTemp()
		defer g.registers.free(resultRegister)

		if arrT, ok := declaration.Type().(types.Array); ok {
			g.allocEmptyArray(arrT)
			g.asm.Pop(resultRegister)
		}
		if strT, ok := declaration.Type().(types.String); ok {
			// allocate size field + len
			g.asm.LoadImmediate(a0, g.platform.RegisterSize())
			g.asm.Call(internalMalloc)

			// save the length of the string (this is always 0)
			temp := g.registers.allocTemp()
			g.asm.AddImmediate(temp, x0, int(strT.Length))
			g.asm.StoreAtOffset(temp, a0, 0)
			g.registers.free(temp)

			// save addr of allocated string in result reg
			g.asm.Move(resultRegister, a0)
		}

		// x0 is always 0
		resultRegister = x0
	}

	g.writeTo(declaration.Identifier, resultRegister, sizeOfResult)
}

// constDeclaration generates code for a constant declaration and
// its constant expression.
//
// Preconditions: The environment must have the stack offset registered for the constant's identifier.
// TODO: improvement: constants are not re-assigned and are read-only therefore, they could
//
//	be hard coded into the .BSS or .DATA section directly at comp-time.
func (g *Generator) constDeclaration(declaration *ast.ConstDeclaration) {
	resultRegister, sizeOfResult := g.expression(declaration.Expression)
	defer g.registers.free(resultRegister)

	g.writeTo(declaration.Identifier, resultRegister, sizeOfResult)
}

// TODO: improvement: more than 8 func params are not supported
//
//	passing params via heap or stack could be a possibility, see ABI.
//
// TODO: improvement: conform to the ABI by always saving all used callee saved registers (s0-s11, if used)
// TODO: improvement: do not waste stack space by only allocating 1 value per 16 bytes
// TODO: improvement: check if the stack is growing into the heap and terminate with error in that case.
func (g *Generator) fnDeclaration(declaration *ast.FuncDeclaration) {
	if len(declaration.Parameters) > 8 {
		// Restricted due to simplicity, the calling convention dictates that
		// if we have more than 8 parameters they must be passed through the stack
		// instead of register a0-a7. This behaviour is currently not supported.
		panic(fmt.Errorf("only a maximum of 8 function arguments are supported"))
	}

	// set the declaration context
	g.declFuncCtx = declaration
	defer func() { g.declFuncCtx = nil }()

	// begin new environment
	g.env = &environment{
		labelCounter: g.env.labelCounter,
		offset:       0, // ra and s0 are always placed at 0 and 4 from sp
		locals:       make(map[string]int),
	}

	// emit code for procedure label
	g.asm.BeginProcedure(declaration.Name())

	// starts at 2x register sizes because ra and s0 needs to be saved
	// as those are callee saved registers
	totalStackSize := 2 * g.platform.RegisterSize()

	// calculate the required stack space for this function, this
	// includes function params and all local variables contained
	// within the function block

	// params are contained in the function scope
	funcScope := g.symbols.DefinedBy(declaration)
	paramEntities := funcScope.Declarations()
	for _, e := range paramEntities {
		size := g.platform.SizeOf(e.Type())
		g.env.register(e.Name(), size)
		totalStackSize += size
	}

	// locals are contained in the block scope
	blockScope := g.symbols.DefinedBy(declaration.Body)
	localEntities := blockScope.AllDeclarations()
	for _, e := range localEntities {
		size := g.platform.SizeOf(e.Type())
		g.env.register(e.Name(), size)
		totalStackSize += size
	}

	// ensure 16 byte alignment of sp, as required by ABI
	if (totalStackSize % 16) != 0 {
		totalStackSize += 16 - (totalStackSize % 16)
	}

	// Knowing how much stack space is required, the function
	// prologue can now be added.
	//
	// The stack will have the following layout:
	//
	// HI	+---------------------------+ <- s0 (frame pointer)
	//		| ...params					| f(param1, param2, ...) -> a0-a8
	//		+---------------------------+
	//		| ...locals					| all local variables/constants
	//		+---------------------------+
	//		| ...callee saved registers	| this includes all "s" registers
	//		+---------------------------+
	//		| return address			|
	// LO   +---------------------------+ <- sp (stack pointer)

	// grow the stack by moving the stack pointer to a lower address
	g.asm.AddImmediate(sp, sp, -(totalStackSize))
	// save the return address and frame pointer so they can later be restored
	g.asm.StoreAtOffset(ra, sp, 0)
	g.asm.StoreAtOffset(s0, sp, g.platform.RegisterSize())
	// set the frame pointer to the highest address of this stack frame
	g.asm.AddImmediate(s0, sp, totalStackSize)

	// save the arguments passed through aX register
	// onto the reserved space on the stack
	if len(declaration.Parameters) > 0 {
		for idx, param := range declaration.Parameters {
			_, entity := funcScope.MustGet(param.Name())
			stackOffset := g.env.lookup(entity.Name())

			// arguments are passed using registers a0-a7
			// Each argument is placed onto the reserved space on the stack.
			// This is only necessary when the variable is ever written to,
			// but since we do not care about optimizations at the moment
			// this optimization will be left out.
			argReg := argRegisters[idx]
			g.asm.StoreAtOffset(argReg, s0, stackOffset)
		}
	}

	// compile the body of the function
	g.block(declaration.Body)

	// insert label which can be used to jump to from return statements
	g.asm.Insert(fmt.Sprintf("ret_%s", declaration.Name()))

	// After the function, the "function epilogue" is needed to restore
	// the callee-saved registers to their original state.
	// This generally includes the following:
	// -> put the original return address back into ra
	// -> put the original frame pointer back into s0
	// -> restore the stack pointer

	// restore return address
	g.asm.LoadFromOffset(ra, sp, 0)
	// restore stack frame pointer
	g.asm.LoadFromOffset(s0, sp, g.platform.RegisterSize())
	// restore stack pointer
	g.asm.AddImmediate(sp, sp, totalStackSize)
	// return from procedure
	g.asm.Return()
}

// assignment generates code for an [*ast.Assignment].
// The code-generation is somewhat special as encountered pointers
// are dereferenced automatically. Currently, pointers can only be
// encountered for index based array access.
func (g *Generator) assignment(assignment *ast.Assignment) {
	// evaluate the value of the assignment
	resultValueReg, sizeOfResult := g.expression(assignment.Value)
	defer func() { g.registers.free(resultValueReg) }()

	// The result can either be a pointer to a value or a value.
	// In case of pointers (e.g. some = a[x]) the result of the expression
	// is the address to the value which needs to be stored rather
	// than the value itself.
	if types.IsPointer(assignment.Value.Type()) {
		// Dereference the pointer by overwriting the resultValueReg
		// with the data stored at the address contained in the resultValueReg.
		g.asm.LoadFromOffset(resultValueReg, resultValueReg, 0)
	}

	// The value is assigned to either a value or a pointer.
	// In case of pointers (e.g. a[x] = value) the value
	// needs to be stored at the address pointed to (*a[x])
	// rather than at the pointer.
	if types.IsPointer(assignment.Left.Type()) {
		// The only case that exists for an assignment to a pointer
		// is if the left hand side is an index expression.
		indexExpr, ok := assignment.Left.(*ast.IndexExpression)
		if !ok {
			panic(fmt.Errorf("left side of assignment is '%T', expected index expression", assignment.Left))
		}

		addrRegister, sizeOfResult := g.indexExpression(indexExpr)
		g.asm.StoreNAtOffset(sizeOfResult, resultValueReg, addrRegister, 0)
		g.registers.free(addrRegister)
	} else {
		// In this case, the left side must be an identifier.
		ident, ok := assignment.Left.(*ast.Identifier)
		if !ok {
			panic(fmt.Errorf("left side of assignment is '%T', expected identifier", assignment.Left))
		}
		g.writeTo(ident, resultValueReg, sizeOfResult)
	}
}

func (g *Generator) ifStatement(ifStatement *ast.IfStatement) {
	lFalse := g.env.provideLabel()
	lAfter := g.env.provideLabel()

	// ignore result size because it's always a bool
	condResReg, _ := g.expression(ifStatement.Condition)

	// jump to false block if condResReg contains false (aka 0)
	g.asm.BranchEQZ(lFalse, condResReg)

	// free the register of the condition for further use
	// this is fine because the register is only used once
	// since the condition is only evaluated once
	g.registers.free(condResReg)

	// code-gen for true block and jump past the rest on completion
	// of the true block
	g.block(ifStatement.Consequence)
	g.asm.Jump(lAfter)

	// TODO: improvement: this label is not always necessary,
	//       in case of more than just if else redundant labels
	//       are inserted
	g.asm.Insert(lFalse)
	// code-gen else and else if in case there are any
	if ifStatement.Alternative != nil {
		g.statement(ifStatement.Alternative)
	}

	g.asm.Insert(lAfter)
}

func (g *Generator) whileStatement(whileStatement *ast.WhileStatement) {
	loopStart := g.env.provideLabel()
	loopEnd := g.env.provideLabel()
	// mark start of loop
	g.asm.Insert(loopStart)

	// evaluate condition, ignore size cause its always bool
	condResReg, _ := g.expression(whileStatement.Condition)

	// jump past loop if the result is false
	g.asm.BranchEQZ(loopEnd, condResReg)
	// free the register as it is no longer
	g.registers.free(condResReg)

	// code-gen body of loop and jump back to condition evaluation
	// after body
	g.statement(whileStatement.Body)
	g.asm.Jump(loopStart)

	// mark end of loop
	g.asm.Insert(loopEnd)
}

// returnStatement generates code for an [*ast.ReturnStatement] by
// compiling its expression, adding its return value into register a0 (if present) and
// jumping to the functions return label (ret_<FUNC_NAME>).
func (g *Generator) returnStatement(returnStatement *ast.ReturnStatement) {
	if returnStatement.Expression != nil {
		resultReg, _ := g.expression(returnStatement.Expression)

		// return values are passed through a0 and a1
		g.asm.Move(a0, resultReg)
		g.registers.free(resultReg)
	}

	// jump to return of function
	g.asm.Jump(fmt.Sprintf("ret_%s", g.declFuncCtx.Name()))
}

// expression is dynamically dispatches the code generation function
// for an [ast.Expression].
func (g *Generator) expression(expression ast.Expression) (Register, size) {
	switch e := expression.(type) {
	case *ast.BasicLiteral:
		return g.basicLiteralExpression(e)
	case *ast.ArrayLiteral:
		return g.arrayLiteralExpression(e), size(g.platform.RegisterSize())
	case *ast.Identifier:
		return g.identifier(e)
	case *ast.PrefixExpression:
		return g.prefixExpression(e)
	case *ast.InfixExpression:
		return g.infixExpression(e)
	case *ast.IndexExpression:
		return g.indexExpression(e)
	case *ast.CallExpression:
		return g.callExpression(e)
		// f() -> result in a0
		// len(arr) -> result in a0

		// we need to be careful here because:
		// f1:
		//	stores stuff in temp register
		//  calls f2
		//  uses temp register <--- error temp register is not persisted across func calls
		// f2:
		//  stores stuff in temp register
		//  returns

	default:
		panic(fmt.Errorf("unsupported expression type: %T", expression))
	}
}

func (g *Generator) basicLiteralExpression(expr *ast.BasicLiteral) (Register, size) {
	// It is safe to use a temporary register as this is a single
	// expression which does not call any functions.
	// The register will not be clobbered.
	targetReg := g.registers.allocTemp()

	val := expr.Value()

	switch val.Type() {
	case constant.Bool:
		// 1 = true, 0 = false
		if b, _ := constant.AsBool(val); b {
			g.asm.LoadImmediate(targetReg, 1)
		} else {
			g.asm.LoadImmediate(targetReg, 0)
		}
	case constant.Int:
		i, _ := constant.AsInt(val)
		g.asm.LoadImmediate(targetReg, i)
	case constant.String:
		v, _ := constant.As[string](val)

		// allocate memory
		g.asm.LoadImmediate(a0, len(v)+g.platform.RegisterSize())
		g.asm.Call(internalMalloc)
		g.asm.Move(targetReg, a0)

		dataTransferReg := g.registers.allocTemp()

		// save the length of the string
		g.asm.LoadImmediate(dataTransferReg, len(v))
		g.asm.StoreAtOffset(dataTransferReg, targetReg, 0)

		// place each string character into allocated memory
		offsetFromTargetAddr := g.platform.RegisterSize()
		for charIdx := range v {
			c := v[charIdx]
			g.asm.LoadImmediate(dataTransferReg, int(c))
			g.asm.StoreNAtOffset(sizeByte, dataTransferReg, targetReg, charIdx+offsetFromTargetAddr)
		}

		g.registers.free(dataTransferReg)
	default:
		panic(fmt.Errorf("unsupported basic literal type: %T", val))
	}

	return targetReg, size(g.platform.SizeOf(expr.Type()))
}

// prefixExpression generates code to evaluate a [*ast.PrefixExpression].
// The result will be in the returned Register.
func (g *Generator) prefixExpression(prefix *ast.PrefixExpression) (Register, size) {
	resultRegister, sizeOfResult := g.expression(prefix.Right)

	// execute operator on whatever is in a0
	switch prefix.Operator {
	case "-":
		g.asm.Neg(resultRegister, resultRegister)
	default:
		panic(fmt.Errorf("prefix operator '%s' not implemented", prefix.Operator))
	}

	return resultRegister, sizeOfResult
}

// infixExpression generates code to evaluate an [*ast.InfixExpression].
// The result is stored in the returned Register.
func (g *Generator) infixExpression(infix *ast.InfixExpression) (Register, size) {
	// evaluate and store the result of the left expression on the stack
	resultLeftReg, _ := g.expression(infix.Left)
	// dereference pointer
	if types.IsPointer(infix.Left.Type()) {
		g.asm.LoadFromOffset(resultLeftReg, resultLeftReg, 0)
	}
	g.asm.Push(resultLeftReg)
	g.registers.free(resultLeftReg)

	// evaluate the right expression
	resultRightReg, _ := g.expression(infix.Right)
	if types.IsPointer(infix.Right.Type()) {
		g.asm.LoadFromOffset(resultRightReg, resultRightReg, 0)
	}

	// pop the result of the left expression back into a register
	leftValReg := g.registers.allocTemp()
	g.asm.Pop(leftValReg)

	switch infix.Operator {
	case "+":
		g.asm.Add(resultRightReg, leftValReg, resultRightReg)
	case "-":
		g.asm.Sub(resultRightReg, leftValReg, resultRightReg)
	case "*":
		g.asm.Mul(resultRightReg, leftValReg, resultRightReg)
	case "/":
		g.asm.Div(resultRightReg, leftValReg, resultRightReg)
	case "%":
		g.asm.Mod(resultRightReg, leftValReg, resultRightReg)
	case "==":
		// TODO: improvement: type based equality
		// if l - r is 0, set result = 1
		g.asm.Sub(resultRightReg, leftValReg, resultRightReg)
		g.asm.SetEqualZero(resultRightReg, resultRightReg)
	case "<":
		g.asm.LessThan(resultRightReg, leftValReg, resultRightReg)
	case "<=":
		g.asm.LessThanOrEqual(resultRightReg, leftValReg, resultRightReg)
	case ">":
		g.asm.GreaterThan(resultRightReg, leftValReg, resultRightReg)
	case ">=":
		g.asm.GreaterThanOrEqual(resultRightReg, leftValReg, resultRightReg)
	case "&&":
		g.asm.And(resultRightReg, leftValReg, resultRightReg)
	case "||":
		g.asm.Or(resultRightReg, leftValReg, resultRightReg)
	case "<<":
		g.asm.ShiftL(resultRightReg, leftValReg, resultRightReg)
	case ">>":
		g.asm.ShiftR(resultRightReg, leftValReg, resultRightReg)
	case "&":
		g.asm.And(resultRightReg, leftValReg, resultRightReg)
	case "|":
		g.asm.Or(resultRightReg, leftValReg, resultRightReg)
	case "^":
		g.asm.XOr(resultRightReg, leftValReg, resultRightReg)
	default:
		panic(fmt.Errorf("infix operator '%s' not implemented", infix.Operator))
	}

	g.registers.free(leftValReg)

	return resultRightReg, size(g.platform.SizeOf(infix.Type()))
}

// identifier generates the code to look up and load the value of the supplied
// identifier into the returned register.
func (g *Generator) identifier(ident *ast.Identifier) (Register, size) {
	tempReg1 := g.registers.allocTemp()
	sizeOfIdent := size(g.platform.SizeOf(ident.Type()))

	scope, _ := g.symbols.Find(ident.Name)
	if symbols.IsGlobal(scope) {
		g.asm.LoadDataAddress(ident.Name, tempReg1)
		g.asm.LoadNFromOffset(sizeOfIdent, tempReg1, tempReg1, 0)
	} else {
		g.asm.LoadNFromOffset(sizeOfIdent, tempReg1, s0, g.env.lookup(ident.Name))
	}

	return tempReg1, sizeOfIdent
}

// arrayLiteralExpression generates code for the supplied [*ast.ArrayLiteral].
// The array literal is heap-allocated and the resulting value is a pointer
// to the array, stored in the returned Register.
func (g *Generator) arrayLiteralExpression(expr *ast.ArrayLiteral) Register {
	// Calculate the full size of the array (including the encoded length field)
	// The memory layout for an array is:
	//        +--------+-------+-----
	// ptr -> | length | elem1 | ...
	//        +--------+-------+-----

	arr, ok := expr.Type().(types.Array)
	if !ok {
		panic(fmt.Errorf("array literal not an array"))
	}

	size := g.platform.RegisterSize() + (g.platform.SizeOf(arr.Element) * len(expr.Elements))

	// allocate memory for the array through built-in malloc and save the returned
	// pointer on the stack
	g.asm.LoadImmediate(a0, size)
	g.asm.Call(internalMalloc)

	// g.asm.DebugAddComment("save array ptr to stack")
	g.asm.Push(a0)

	// g.asm.DebugAddComment("save length to array in heap")

	// write the length to the reserved memory
	lengthReg := g.registers.allocTemp()
	g.asm.LoadImmediate(lengthReg, len(expr.Elements))
	g.asm.StoreAtOffset(lengthReg, a0, 0)
	g.registers.free(lengthReg)

	// g.asm.DebugAddComment("write data loop")

	// write each element into the reserved memory
	for idx, elem := range expr.Elements {
		// evaluate the elements expression and save the
		// result into the next slot of the allocated memory
		elemResultRegister, sizeOfElem := g.expression(elem)

		// pop the pointer to the array off the stack
		// g.asm.DebugAddComment("get array ptr from stack")
		baseAddrReg := g.registers.allocTemp()
		g.asm.Pop(baseAddrReg)

		// retrieve the type of the element
		elemSize := g.platform.SizeOf(elem.Type())
		lenSize := g.platform.SizeOf(types.NewInt())

		// length of the array is stored at offset 0
		//g.asm.StoreAtOffset(elemResultRegister, baseAddrReg, lenSize+(elemSize*idx))
		g.asm.StoreNAtOffset(sizeOfElem, elemResultRegister, baseAddrReg, lenSize+(elemSize*idx))

		// push the address back onto the stack
		// g.asm.DebugAddComment("push array ptr to stack")
		g.asm.Push(baseAddrReg)

		// free the register for further use
		g.registers.free(elemResultRegister)
		g.registers.free(baseAddrReg)
	}

	// pointer to array is no longer needed after writing all elements
	resultAddrReg := g.registers.allocTemp()
	g.asm.Pop(resultAddrReg)

	return resultAddrReg
}

// indexExpression generates code for the supplied [*ast.IndexExpression].
// The resulting register will always contain a pointer to the specified element.
// Additionally, bounds checking is implemented for each array access.
//
//	TODO: improvement: bounds checking on array access.
func (g *Generator) indexExpression(expr *ast.IndexExpression) (Register, size) {
	// evaluate the left hand side of the expression and save the result on the stack.
	// This ensures that the register is not clobbered by the evaluation of the index expression.
	// g.asm.DebugAddComment("eval left side of index expression")
	arrayAddrReg, _ := g.expression(expr.Left)

	// When dealing with multiple dimensions Left might contain another ast.IndexExpression.
	// e.g. a 2D array (arr [][]int), accessed by arr[0] will result in *[]T therefore,
	// the result must be dereferenced, so that the subsequent index-based access
	// returns the address of arr[0] + offset and not arr + offset
	if types.IsPointer(expr.Left.Type()) {
		// g.asm.DebugAddComment("dereference pointer")
		g.asm.LoadFromOffset(arrayAddrReg, arrayAddrReg, 0)
	}

	g.asm.Push(arrayAddrReg)
	g.registers.free(arrayAddrReg)

	// ignore size, index is always int
	arrayIndexReg, _ := g.expression(expr.Index)

	// allocate a new temporary register to pop the array pointer back into
	arrayAddrReg = g.registers.allocTemp()
	g.asm.Pop(arrayAddrReg)

	// calculate size of the length
	arrayLenSize := g.platform.SizeOf(types.NewInt())
	elementSize := g.platform.SizeOf(expr.Type().Underlying())

	// calculate address, the offset is +1 because at 0 we store the length of the array
	elemSizeReg := g.registers.allocTemp()

	// pointer to element = (size of elem * index) + size of length
	g.asm.LoadImmediate(elemSizeReg, elementSize)
	g.asm.Mul(arrayIndexReg, arrayIndexReg, elemSizeReg)
	g.asm.Add(arrayAddrReg, arrayAddrReg, arrayIndexReg)
	g.asm.AddImmediate(arrayAddrReg, arrayAddrReg, arrayLenSize)

	g.registers.free(arrayIndexReg)
	g.registers.free(elemSizeReg)

	return arrayAddrReg, size(g.platform.SizeOf(expr.Type().Underlying()))
}

func (g *Generator) callExpression(expr *ast.CallExpression) (Register, size) {
	if len(expr.Arguments) > 8 {
		panic(fmt.Errorf("passing more than 8 arguments is currently not supported"))
	}

	// handle special functions provided by the compiler such as print
	if builtInFunc, ok := findBuiltIn(expr.Function.Name); ok {
		builtInFunc.rewriteCall(expr)
	}

	// backwards due to stack
	for i := len(expr.Arguments) - 1; i >= 0; i-- {
		argExpr := expr.Arguments[i]

		argReg, argSize := g.expression(argExpr)

		// deref if necessary (e.g. x[0] is a ptr to elem, we want the elem)
		if types.IsPointer(argExpr.Type()) {
			//g.asm.LoadFromOffset(argReg, argReg, 0)
			g.asm.LoadNFromOffset(argSize, argReg, argReg, 0)
		}

		// Save onto stack to avoid clobbering through function arguments.
		// Unfortunately, arguments cannot be placed into a0...a7 directly
		// as their expressions might be other function calls, potentially
		// overwriting the a0...a7 registers.
		g.asm.Push(argReg)
		// free temporary expression register, so it can be re-used
		// by the next expression
		g.registers.free(argReg)
	}

	// place expression results in a0...a7
	for idx := 0; idx < len(expr.Arguments); idx++ {
		g.asm.Pop(argRegisters[idx])
	}

	// function call
	g.asm.Call(expr.Function.Name)

	// function result
	if expr.Type().Equals(types.NewVoid()) {
		return x0, size(g.platform.RegisterSize()) // no return value
	} else {
		return a0, size(g.platform.SizeOf(expr.Type())) // default return register
	}
}

// writeTo writes the value of the supplied "from" Register to
// the address of the identifier.
func (g *Generator) writeTo(identifier *ast.Identifier, from Register, sizeOfSource size) {
	declScope, decl := g.symbols.Find(identifier.Name)
	if decl == nil {
		panic(fmt.Errorf("could not find declaration of %v", identifier.Name))
	}

	// g.asm.DebugAddComment("write result to identifier")
	if symbols.IsGlobal(declScope) {
		// decl is global and located in the .data section
		tempReg := g.registers.allocTemp()
		g.asm.LoadDataAddress(decl.Name(), tempReg)
		g.asm.StoreNAtOffset(sizeOfSource, from, tempReg, 0)
		g.registers.free(tempReg)
	} else {
		// decl is local and stack allocated
		offset := g.env.lookup(decl.Name())
		g.asm.StoreAtOffset(from, s0, offset)
	}
}

// allocArray allocates space for an array
// dst is the register which will contain the address of the array
func (g *Generator) allocEmptyArray(arrT types.Array) {
	size := g.platform.RegisterSize() + (g.platform.SizeOf(arrT.Element) * int(arrT.Length))

	// allocate size field + len
	g.asm.LoadImmediate(a0, size)
	g.asm.Call(internalMalloc)

	// save the length of the array
	temp := g.registers.allocTemp()
	g.asm.AddImmediate(temp, x0, int(arrT.Length))
	g.asm.StoreAtOffset(temp, a0, 0)
	g.registers.free(temp)

	// save addr of allocated array on the stack
	g.asm.Push(a0)

	switch t := arrT.Element.(type) {
	case types.Array:
		sizeOfElem := g.platform.SizeOf(arrT.Element)
		for i := 0; i < int(arrT.Length); i++ {
			g.allocEmptyArray(t)

			dstArrAddr := g.registers.allocTemp()
			g.asm.Pop(dstArrAddr)

			originalArrAddr := g.registers.allocTemp()
			g.asm.Pop(originalArrAddr)

			g.asm.StoreAtOffset(dstArrAddr, originalArrAddr, i*sizeOfElem+g.platform.RegisterSize())

			g.asm.Push(originalArrAddr)

			g.registers.free(originalArrAddr)
			g.registers.free(dstArrAddr)
		}
	case types.String:
		// allocating empty strings would be possible
		// but is not doing much at the moment as
		// they are immutable
		return
	default:
		// non-heap allocated data types do not need
		// pre-allocation, it is assumed that 0x0 is a
		// valid default value for those types
		return
	}
}

func emitInt(p Platform, v int) emitData {
	switch p.RegisterSize() {
	case 4:
		return emitData{
			kind:  ".word",
			value: fmt.Sprintf("%d", v),
		}
	case 8:
		return emitData{
			kind:  ".dword",
			value: fmt.Sprintf("%d", v),
		}
	default:
		panic(fmt.Errorf("unknown register size %d", p.RegisterSize()))
	}
}

func emitZero(p Platform, decl ast.Declaration) emitData {
	switch decl.Type().(type) {
	case types.Int:
		return emitInt(p, 0)
	case types.Bool:
		return emitInt(p, 0)
	case types.String:
		return emitData{kind: emitString, value: ""}
	case types.Array:
		return emitInt(p, 0)
	default:
		panic(fmt.Errorf("cannot emit a value for %v", decl.Type()))
	}
}

func emitConst(p Platform, c ast.ConstExpression) emitData {
	switch c.Type().(type) {
	case types.Int:
		intVal, _ := constant.AsInt(c.Value())
		return emitInt(p, intVal)
	case types.Bool:
		v := 0
		if b, _ := constant.AsBool(c.Value()); b {
			v = 1
		}
		return emitInt(p, v)
	case types.String:
		v, _ := constant.As[string](c.Value())
		return emitData{kind: emitString, value: v}
	default:
		panic(fmt.Errorf("cannot emit a constant value for %v", c))
	}
}
