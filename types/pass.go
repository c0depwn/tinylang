package types

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	ext "github.com/c0depwn/tinylang/pkg/slices"
	"github.com/c0depwn/tinylang/token"
	"io"
	"slices"
)

// Symbols provides ast.Declaration related operations
// required during type checking.
type Symbols interface {
	// FindInScope provides the [ast.Declaration] for the supplied identifier.
	// If no [ast.Declaration] is found, nil must be returned.
	FindInScope(identifier string) ast.Declaration
	// Before is called pre-traversal of the [ast.Node].
	Before(ast.Node)
	// After is called post-traversal of the [ast.Node]
	After(ast.Node)
}

type AnalyzeOption func(*Options)

func WithTraversalTrace(out io.Writer) AnalyzeOption {
	return func(o *Options) { o.tracer = newTracer(out) }
}

type Options struct {
	tracer trace
}

func NewOptions() *Options {
	return &Options{
		tracer: dummyTracer{},
	}
}

func (opt *Options) Apply(options ...AnalyzeOption) *Options {
	for _, o := range options {
		o(opt)
	}
	return opt
}

// Analyze type related semantics. If no error is returned all relevant AST nodes will
// be annotated with their type information (the nodes T field will not be nil).
func Analyze(f *ast.File, symbolInfo Symbols, opts ...AnalyzeOption) error {
	options := NewOptions().Apply(opts...)

	// add default built-in functions
	builtInFuncs = append(
		builtInFuncs,
		provideBuiltInLen(),
		provideBuiltInPrint(),
		provideBuiltInPrintLn(),
	)

	// First, type information of declared functions must be
	// collected as they may be used in the global context
	// such as in global variable initialization.
	options.tracer.info("> traverse function declarations")
	forEach(f.Declarations, func(fDecl *ast.FuncDeclaration) {
		options.tracer.info(fDecl.Name())

		// extract param types
		paramTypes := ext.Map(fDecl.Parameters, func(param *ast.Param) ast.Type {
			param.T = FromTypeIdentifier(param.TypeID)
			return param.T
		})
		// annotate the node with type information
		fDecl.T = NewFunction(
			FromTypeIdentifier(fDecl.Result),
			paramTypes,
		)
	})

	// Next, collect type information of global variables and constants.
	// This will not yet check against any potential initialization
	// expressions but rather only register the declared type as
	// the initialization expression might depend on other globals.
	options.tracer.info("> traverse global variables")
	forEach(f.Declarations, func(varDecl *ast.VarDeclaration) {
		options.tracer.info(varDecl.Name())
		varDecl.T = FromTypeIdentifier(varDecl.TypeName)
	})
	options.tracer.info("> traverse globals constants")
	forEach(f.Declarations, func(constDecl *ast.ConstDeclaration) {
		options.tracer.info(constDecl.Name())
		constDecl.T = FromConstant(constDecl.Expression.Value())
	})

	typeChecker := makeChecker(symbolInfo)
	var err error

	// Finally, traverse everything propagating types upwards and
	// checking types post-traversal.
	// In case of var/const declarations, the expression is traversed.
	// In case of func declarations, the body is traversed.
	ast.Inspect(f, func(n ast.Node) bool {
		if err != nil {
			return false
		}

		if n == nil {
			// remove from traversal state
			n = typeChecker.pop()

			// debugging
			options.tracer.post(n)

			// The type checking is done once node traversal has
			// completed as the type information of child nodes
			// is only available post traversal.
			err = typeChecker.checkDispatch(n)

			return true
		}

		// debugging
		options.tracer.pre(n)
		// add to traversal state
		typeChecker.push(n)
		// propagate type information of node to parents
		typeChecker.propagate(n)

		return true
	})

	return err
}

type traversal struct {
	stack []ast.Node
}

func (t *traversal) findDeclarationCtx() ast.Declaration {
	for i := len(t.stack) - 1; i > 0; i-- {
		decl, ok := t.stack[i].(ast.Declaration)
		if ok {
			return decl
		}
	}
	return nil
}

func (t *traversal) push(n ast.Node) {
	t.stack = append(t.stack, n)
}

func (t *traversal) pop() ast.Node {
	n := t.stack[len(t.stack)-1]
	t.stack = t.stack[:len(t.stack)-1]
	return n
}

type checker struct {
	symbols Symbols
	*traversal
}

func makeChecker(symbols Symbols) checker {
	return checker{
		symbols:   symbols,
		traversal: &traversal{make([]ast.Node, 0)},
	}
}

func (c checker) push(n ast.Node) {
	c.traversal.push(n)
	c.symbols.Before(n)
}

func (c checker) pop() ast.Node {
	popped := c.traversal.pop()
	c.symbols.After(popped)
	return popped
}

func (c checker) propagate(n ast.Node) {
	// Leaf nodes do not need their types checked but rather
	// propagate type information upwards in the AST.
	switch node := n.(type) {
	case *ast.VarDeclaration:
		// as a special case local var declarations need their type set
		// early to avoid issues when looking up an identifiers type
		// e.g. var x int = f(x); will cause a nil pointer exception
		// because the type of the declaration is not yet present when first
		// traversing the initialization expression.
		node.T = FromTypeIdentifier(node.TypeName)

	case *ast.BasicLiteral:
		node.T = FromConstant(node.Value())

	// ast.Param is excluded from this because it has already
	// been annotated with its type during the global func decl pass.
	case *ast.Param:

	case *ast.Identifier:
		if slices.ContainsFunc(builtInFuncs, byIdentifier(node)) {
			return
		}
		if token.IsReservedKeyword(node.Name) {
			return
		}

		d := c.symbols.FindInScope(node.Name)
		if d == nil {
			panic(fmt.Errorf("declaration for name '%s' nil", node.Name))
		}
		if d.Type() == nil {
			panic(fmt.Errorf("declaration type for name '%s' nil", node.Name))
		}
		node.T = d.Type()
	}
}

// checkDispatch dynamically calls the type checking function
// depending on the [ast.Node]'s type.
// Any type checking relevant [ast.Node] has its own check
// function, so it can provide more specific errors.
func (c checker) checkDispatch(n ast.Node) error {
	var err error
	switch node := n.(type) {
	case *ast.VarDeclaration:
		err = c.checkVar(node)
	case *ast.ConstDeclaration:
		err = c.checkConst(node)
	case *ast.Assignment:
		err = c.checkAssignment(node)
	case *ast.IfStatement:
		err = c.checkIfStatement(node)
	case *ast.WhileStatement:
		err = c.checkWhileStatement(node)
	case *ast.ReturnStatement:
		err = c.checkReturn(node)
	case *ast.ExpressionStatement:
		err = c.checkExpressionStatement(node)
	case *ast.ArrayLiteral:
		err = c.checkArrayLiteral(node)
	case *ast.PrefixExpression:
		err = c.checkPrefixExpression(node)
	case *ast.InfixExpression:
		err = c.checkInfixExpression(node)
	case *ast.IndexExpression:
		err = c.checkIndexExpression(node)
	case *ast.CallExpression:
		err = c.checkCallExpression(node)
	}
	return err
}

const varErrInvalidInit = "var type does not match expression"

func (c checker) checkVar(varDecl *ast.VarDeclaration) error {
	// locals do not have their declared type set yet
	if varDecl.T == nil {
		varDecl.T = FromTypeIdentifier(varDecl.TypeName)
	}

	if varDecl.Expression == nil {
		return nil
	}

	exprT := varDecl.Expression.Type()

	// must dereference, this only happens for index expr at the moment
	if IsPointer(exprT) {
		exprT = exprT.Underlying()
	}

	// the expressions type must match the variables declared type
	if varDecl.Type().Equals(exprT) {
		// overwrite the declaration type with the expression type as it
		// potentially contains more information (e.g. length of a string)
		varDecl.T = exprT
		return nil
	}

	return c.wrap(
		varDecl.Position(),
		newTypeError(varErrInvalidInit).
			WithExpect(varDecl.Type()).
			WithActual(exprT),
	)
}

const constErrInvalidInit = "const type does not match expression"

func (c checker) checkConst(constDecl *ast.ConstDeclaration) error {
	declT := FromTypeIdentifier(constDecl.TypeName)
	exprT := FromConstant(constDecl.Expression.Value())

	if declT.Equals(exprT) {
		// Attach the type information.
		constDecl.T = exprT
		return nil
	}

	return c.wrap(
		constDecl.Position(),
		newTypeError(constErrInvalidInit).
			WithExpect(declT).
			WithActual(exprT),
	)
}

const assignmentErrIncompatible = "incompatible assignment"

func (c checker) checkAssignment(assignment *ast.Assignment) error {
	leftT := assignment.Left.Type()
	valueT := assignment.Value.Type()

	// When assigning to a pointer the underlying T
	// is of relevance rather than ast.Type Pointer.
	// e.g. In x[0] = 1; x[0] is a *T, in type checking, T is relevant not *T
	if IsPointer(leftT) {
		leftT = leftT.Underlying()
	}
	if IsPointer(valueT) {
		valueT = valueT.Underlying()
	}

	if valueT.Equals(leftT) {
		return nil
	}

	return c.wrap(
		assignment.Position(),
		newTypeError(assignmentErrIncompatible).
			WithExpect(leftT).
			WithActual(valueT),
	)
}

const ifStmtErrInvalidCondition = "invalid if condition"

func (c checker) checkIfStatement(statement *ast.IfStatement) error {
	conditionExprT := statement.Condition.Type()

	// ensure the expression result is a bool
	if conditionExprT.Equals(NewBool()) {
		return nil
	}

	return c.wrap(
		statement.Position(),
		newTypeError(ifStmtErrInvalidCondition).
			WithExpect(NewBool()).
			WithActual(conditionExprT),
	)
}

const whileStmtErrInvalidCondition = "invalid loop condition"

func (c checker) checkWhileStatement(statement *ast.WhileStatement) error {
	conditionExprT := statement.Condition.Type()

	// ensure the expression results in a bool
	if conditionExprT.Equals(NewBool()) {
		return nil
	}

	// TODO: improvement: since for loops are converted to while some might be missing the Position()
	//	     it must be ensured that the position of every converted loop is set during parsing
	return c.wrap(
		statement.Position(),
		newTypeError(whileStmtErrInvalidCondition).
			WithExpect(NewBool()).
			WithActual(conditionExprT),
	)
}

const retStmtErrIncompatible = "incompatible return value"

func (c checker) checkReturn(statement *ast.ReturnStatement) error {
	decl := c.findDeclarationCtx()
	if decl == nil {
		// panic because the symbol must be resolvable at this point
		panic("expected declaration to exist")
	}

	fDecl, ok := decl.(*ast.FuncDeclaration)
	if !ok {
		// panic because the return can only appear
		// within a function declaration. This indicates
		// a bug somewhere else.
		panic(fmt.Errorf("expected function declaration, got '%T'", decl))
	}

	var exprT ast.Type = NewVoid()
	if statement.Expression != nil {
		exprT = statement.Expression.Type()
	}

	resultT := fDecl.Type().(Function).Result

	if IsPointer(resultT) {
		resultT = resultT.Underlying()
	}

	if !resultT.Equals(exprT) {
		return c.wrap(
			fDecl.Position(),
			newTypeError(retStmtErrIncompatible).
				WithExpect(resultT).
				WithActual(exprT),
		)
	}

	return nil
}

func (c checker) checkExpressionStatement(statement *ast.ExpressionStatement) error {
	callExpr, ok := statement.Expression.(*ast.CallExpression)
	if !ok {
		return nil
	}

	var resultT ast.Type

	if idx := slices.IndexFunc(builtInFuncs, byIdentifier(callExpr.Function)); idx >= 0 {
		resultT = builtInFuncs[idx].provideResultT()
	} else {
		decl := c.symbols.FindInScope(callExpr.Function.Name)
		if decl == nil {
			panic("expected function declaration to exist")
		}

		funcT, ok := decl.Type().(Function)
		if !ok {
			panic(fmt.Errorf("expected function declaration, got '%T'", decl))
		}

		resultT = funcT.Result
	}

	if resultT.Equals(NewVoid()) {
		return nil
	}

	return c.wrap(statement.Position(), fmt.Errorf("function call result ignored"))
}

const (
	arrayLitErrTooManyElements         = "array literal has too many elements"
	arrayLitErrIncorrectElementTypeFmt = "incorrect array element type at index %d"
)

func (c checker) checkArrayLiteral(array *ast.ArrayLiteral) error {
	literalType := FromTypeIdentifier(array.TypeID)
	literalArrType, ok := literalType.(Array)
	if !ok {
		// panic because this must be prevented by the parser
		panic("array literals type is not an array")
	}

	// The literal must not necessarily contain the number of elements
	// defined by its type (e.g. [2]int{} is allowed).
	if literalArrType.Length < uint32(len(array.Elements)) {
		return c.wrap(array.Position(), newTypeError(arrayLitErrTooManyElements))
	}

	// Ensure that the array literals elements match the defined type.
	for i, element := range array.Elements {
		if !element.Type().Equals(literalArrType.Element) {
			tErr := newTypeError(fmt.Sprintf(arrayLitErrIncorrectElementTypeFmt, i)).
				WithExpect(literalArrType.Element).
				WithActual(element.Type())
			return c.wrap(array.Position(), tErr)
		}
	}

	array.T = literalType
	return nil
}

const prefixErrOperatorIncompatible = "incompatible prefix operator"

func (c checker) checkPrefixExpression(expr *ast.PrefixExpression) error {
	t := expr.Right.Type().Underlying()

	if !hasPrefixOperator(token.Type(expr.Operator), t) {
		return c.wrap(expr.Position(), newTypeError(prefixErrOperatorIncompatible))
	}

	expr.T = t
	return nil
}

const (
	infixErrOperandMismatch      = "type of operands must match"
	infixErrOperatorIncompatible = "incompatible infix operator"
)

func (c checker) checkInfixExpression(expr *ast.InfixExpression) error {
	rightT := expr.Right.Type().Underlying()
	leftT := expr.Left.Type().Underlying()

	// left and right type must match
	if !rightT.Equals(leftT) {
		return c.wrap(
			expr.Position(),
			newTypeError(infixErrOperandMismatch).
				WithExpect(leftT).
				WithActual(rightT))
	}

	// the operator must be defined for the type
	if !hasInfixOperator(token.Type(expr.Operator), leftT) {
		return c.wrap(expr.Position(), newTypeError(infixErrOperatorIncompatible))
	}

	// TODO: improvement: add a test for this
	// comparisons like >,<,==,<=,>= always result in bool
	if isComparison(token.Type(expr.Operator)) {
		expr.T = NewBool()
		return nil
	}

	expr.T = rightT
	return nil
}

const (
	indexExprErrNotArray     = "indexing not supported on type"
	indexExprErrInvalidIndex = "bad index expression"
)

func (c checker) checkIndexExpression(expr *ast.IndexExpression) error {
	leftT := expr.Left.Type()

	if IsPointer(leftT) {
		leftT = leftT.Underlying()
	}

	// The result of the left expression must always be an array.
	// TODO: improvement: allow strings to access chars e.g. mystr = 'abc'; mystr[0] => 'a'
	arrT, ok := leftT.(Array)
	if !ok {
		return c.wrap(
			expr.Left.Position(),
			newTypeError(indexExprErrNotArray),
		)
	}

	indexT := expr.Index.Type()
	if !indexT.Equals(NewInt()) {
		return c.wrap(
			expr.Index.Position(),
			newTypeError(indexExprErrInvalidIndex).
				WithExpect(NewInt()).
				WithActual(indexT),
		)
	}

	// Annotate expression node with type information.
	// The result of an index expression is an address pointing to
	// an element rather than the element type itself.
	expr.T = NewPtr(arrT.Element)
	return nil
}

const (
	callExprErrInvalidNumOfArgument = "incorrect number of arguments"
	callExprErrInvalidArgumentFmt   = "incorrect type of argument %d"
)

func (c checker) checkCallExpression(expr *ast.CallExpression) error {
	// built-in functions can supply custom type checking logic
	if idx := slices.IndexFunc(builtInFuncs, func(f builtInFunc) bool {
		return f.identifier == expr.Function.Name
	}); idx >= 0 {
		fDecl := builtInFuncs[idx]
		if err := fDecl.checkCall(expr.Arguments); err != nil {
			return err
		}
		expr.T = fDecl.provideResultT()
		return nil
	}

	// lookup signature of func declaration
	f := c.symbols.FindInScope(expr.Function.Name).(*ast.FuncDeclaration)
	if f == nil {
		// panic because at this point the existence of all declarations must be guaranteed
		panic("function not found")
	}

	// ensure number of arguments is correct
	if len(f.Parameters) != len(expr.Arguments) {
		return c.wrap(
			expr.Position(),
			newTypeError(callExprErrInvalidNumOfArgument),
		)
	}

	// ensure the arguments match the function signature
	for i, parameter := range f.Parameters {
		argExpr := expr.Arguments[i]
		argT := argExpr.Type()

		// dereference pointers for example on x[i] the element is passed rather
		// than a pointer to that element
		if IsPointer(argT) {
			argT = argT.Underlying()
		}

		expectT := FromTypeIdentifier(parameter.TypeID)
		if !argT.Equals(expectT) {
			return c.wrap(
				expr.Position(),
				newTypeError(fmt.Sprintf(callExprErrInvalidArgumentFmt, i+1)).
					WithExpect(expectT).
					WithActual(argT),
			)
		}
	}

	// the expressions type is the result type of the function
	expr.T = FromTypeIdentifier(f.Result)
	return nil
}

func (c checker) wrap(pos token.Position, err error) error {
	return fmt.Errorf("%d:%d: %w", pos.Row, pos.Col, err)
}

func forEach[T ast.Declaration](ds []ast.Declaration, f func(T)) {
	for _, d := range ds {
		if specific, ok := d.(T); ok {
			f(specific)
		}
	}
}
