package types

import (
	"errors"
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	"testing"
)

func withStatements(stmts ...ast.Statement) fOpt {
	return func(decl *ast.FuncDeclaration) {
		decl.Body.Statements = stmts
	}
}

func newBasicV(v any) *ast.BasicLiteral {
	bl := &ast.BasicLiteral{}
	bl.SetValue(constant.Make(v))
	return bl
}

func newBasicT(id string) ast.TypeIdentifier {
	return &ast.BasicTypeName{Name: id}
}

func newArrayT(length int, elemT ast.TypeIdentifier) *ast.ArrayType {
	bl := &ast.BasicLiteral{}
	bl.SetValue(constant.Make(length))

	return &ast.ArrayType{
		Len:         bl,
		ElementType: elemT,
	}
}

func withResult(t ast.TypeIdentifier) fOpt {
	return func(decl *ast.FuncDeclaration) {
		decl.Result = t
	}
}

func withParam(id string, t ast.TypeIdentifier) fOpt {
	return func(decl *ast.FuncDeclaration) {
		decl.Parameters = append(decl.Parameters, &ast.Param{
			Identifier: &ast.Identifier{Name: id},
			TypeID:     t,
		})
	}
}

type fOpt func(*ast.FuncDeclaration)

func newFunc(id string, opts ...fOpt) *ast.FuncDeclaration {
	f := &ast.FuncDeclaration{
		Identifier: &ast.Identifier{Name: id},
		Parameters: make([]*ast.Param, 0),
		Body:       &ast.Block{},
	}
	for _, o := range opts {
		o(f)
	}
	return f
}

func newExprStmt(expression ast.Expression) *ast.ExpressionStatement {
	return &ast.ExpressionStatement{Expression: expression}
}

// symbols implements Symbols for testing.
// It is restricted to a single global scope.
type symbols struct {
	m map[string]ast.Declaration
}

func newSymbols() symbols {
	return symbols{m: make(map[string]ast.Declaration)}
}

func (s symbols) FindInScope(id string) ast.Declaration {
	decl, ok := s.m[id]
	if !ok {
		return nil
	}
	return decl
}

func (s symbols) Before(ast.Node) {}

func (s symbols) After(ast.Node) {}

func (s symbols) register(decl ast.Declaration) {
	fmt.Printf("registering decl %s\n", decl)

	s.m[decl.Name()] = decl
}

// -- statements

func TestAnalyze_Var(t *testing.T) {
	v := newVar(
		newID("x"),
		newBasicT("int"),
		newBasicV(false),
	)

	expectErr := newTypeError(varErrInvalidInit).
		WithExpect(NewInt()).
		WithActual(NewBool())

	s := newSymbols()
	s.register(v)

	err := Analyze(newFile(v), s)
	if err == nil {
		t.Fatalf("expecting error, got nil")
	}
	if !errors.Is(err, expectErr) {
		t.Fatalf("expecting error '%v', got '%v'", expectErr, err)
	}
}

func TestAnalyze_Const(t *testing.T) {
	v := newConst(
		newID("x"),
		newBasicT("int"),
		newBasicV(false),
	)

	expectErr := newTypeError(constErrInvalidInit).
		WithExpect(NewInt()).
		WithActual(NewBool())

	s := newSymbols()
	s.register(v)

	err := Analyze(newFile(v), s)
	if err == nil {
		t.Fatalf("expecting error, got nil")
	}

	if !errors.Is(err, expectErr) {
		t.Fatalf("expecting error '%v', got '%v'", expectErr, err)
	}
}

func TestAnalyze_AssignmentInvalid(t *testing.T) {
	x := newVar(newID("x"), newBasicT("int"), newBasicV(1))
	y := newVar(newID("y"), newBasicT("bool"), newBasicV(false))
	a := &ast.Assignment{Left: newID("x"), Value: newID("y")}
	mainF := newFunc("main", withStatements(x, y, a))

	s := newSymbols()
	s.register(x)
	s.register(y)
	s.register(mainF)

	f := newFile(mainF)

	expectErr := newTypeError(assignmentErrIncompatible).
		WithExpect(NewInt()).
		WithActual(NewBool())

	err := Analyze(f, s)
	if err == nil {
		t.Fatalf("expecting error, got nil")
	}
	if !errors.Is(err, expectErr) {
		t.Fatalf("expecting error '%v', got '%v'", expectErr, err)
	}
}

func TestAnalyze_IfInvalid(t *testing.T) {
	ifStmt := &ast.IfStatement{Condition: newBasicV(1), Consequence: &ast.Block{}}
	mainF := newFunc("main", withStatements(ifStmt))

	s := newSymbols()
	s.register(mainF)

	f := newFile(mainF)

	expectErr := newTypeError(ifStmtErrInvalidCondition).
		WithExpect(NewBool()).
		WithActual(NewInt())

	err := Analyze(f, s)
	if err == nil {
		t.Fatalf("expecting error, got nil")
	}
	if !errors.Is(err, expectErr) {
		t.Fatalf("expecting error '%v', got '%v'", expectErr, err)
	}
}

func TestAnalyze_WhileInvalid(t *testing.T) {
	whileStmt := &ast.WhileStatement{Condition: newBasicV(1), Body: &ast.Block{}}
	mainF := newFunc("main", withStatements(whileStmt))

	s := newSymbols()
	s.register(mainF)

	f := newFile(mainF)

	expectErr := newTypeError(whileStmtErrInvalidCondition).
		WithExpect(NewBool()).
		WithActual(NewInt())

	err := Analyze(f, s)
	if err == nil {
		t.Fatalf("expecting error, got nil")
	}
	if !errors.Is(err, expectErr) {
		t.Fatalf("expecting error '%v', got '%v'", expectErr, err)
	}
}

func TestAnalyze_ReturnInvalid(t *testing.T) {
	cases := []struct {
		name   string
		f      *ast.FuncDeclaration
		expect error
	}{
		{
			// fn sum(a int, b int) int { return; }
			name: "missing return type",
			f: newFunc(
				"sum",
				withParam("a", newBasicT("int")),
				withParam("b", newBasicT("int")),
				withResult(newBasicT("int")),
				withStatements(&ast.ReturnStatement{Expression: nil}),
			),
			expect: newTypeError(retStmtErrIncompatible).
				WithExpect(NewInt()).
				WithActual(NewVoid()),
		},
		{
			// fn sum(a int, b int) int { return true; }
			name: "bad return type",
			f: newFunc(
				"sum",
				withParam("a", newBasicT("int")),
				withParam("b", newBasicT("int")),
				withResult(newBasicT("int")),
				withStatements(&ast.ReturnStatement{Expression: newBasicV(true)}),
			),
			expect: newTypeError(retStmtErrIncompatible).
				WithExpect(NewInt()).
				WithActual(NewBool()),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			s := newSymbols()
			s.register(tc.f)

			f := newFile(tc.f)

			err := Analyze(f, s)
			if err == nil {
				t.Fatalf("expecting error, got nil")
			}
			if !errors.Is(err, tc.expect) {
				t.Fatalf("expecting error '%v', got '%v'", tc.expect, err)
			}
		})
	}
}

// -- expressions

func TestAnalyze_ArrayLiteralInvalid(t *testing.T) {
	cases := []struct {
		decl   *ast.VarDeclaration
		expect error
	}{
		// var x [1]int = [1]int{1,2}
		{
			decl: newVar(
				newID("x"),
				newArrayT(1, newBasicT("int")),
				&ast.ArrayLiteral{
					TypeID: newArrayT(1, newBasicT("int")),
					Elements: []ast.Expression{
						newBasicV(1),
						newBasicV(2),
					},
				},
			),
			expect: newTypeError(arrayLitErrTooManyElements),
		},
		// var x [2][2]int = [2][2]int{1,2}
		{
			decl: newVar(
				newID("x"),
				newArrayT(2, newArrayT(2, newBasicT("int"))),
				&ast.ArrayLiteral{
					TypeID: newArrayT(2, newArrayT(2, newBasicT("int"))),
					Elements: []ast.Expression{
						newBasicV(1),
						newBasicV(2),
					},
				},
			),
			expect: newTypeError(fmt.Sprintf(arrayLitErrIncorrectElementTypeFmt, 0)).
				WithExpect(NewArray(uint32(2), NewInt())).
				WithActual(NewInt()),
		},
	}
	for i, tc := range cases {
		tc := tc
		t.Run(fmt.Sprintf("case-%d", i), func(t *testing.T) {
			s := newSymbols()
			s.register(tc.decl)

			f := newFile(tc.decl)

			err := Analyze(f, s)

			if err == nil {
				t.Fatalf("expecting error, got nil")
			}

			if !errors.Is(err, tc.expect) {
				t.Fatalf("expecting error '%v', got '%v'", tc.expect, err)
			}
		})
	}
}

func TestAnalyze_ArrayLiteralValid(t *testing.T) {
	cases := []struct {
		decl *ast.VarDeclaration
	}{
		// var x [1]int = [1]int{}
		{
			decl: newVar(
				newID("x"),
				newArrayT(1, newBasicT("int")),
				&ast.ArrayLiteral{
					Elements: []ast.Expression{},
					TypeID:   newArrayT(1, newBasicT("int")),
				},
			),
		},
		// var x [1][1]int = [1]int{}
		{
			decl: newVar(
				newID("x"),
				newArrayT(1, newArrayT(1, newBasicT("int"))),
				&ast.ArrayLiteral{
					Elements: []ast.Expression{},
					TypeID:   newArrayT(1, newArrayT(1, newBasicT("int"))),
				},
			),
		},
		// var x [1][1]int = [1][1]int{[1]int{}}
		{
			decl: newVar(
				newID("x"),
				newArrayT(1, newArrayT(1, newBasicT("int"))),
				&ast.ArrayLiteral{
					TypeID: newArrayT(1, newArrayT(1, newBasicT("int"))),
					Elements: []ast.Expression{
						&ast.ArrayLiteral{
							TypeID:   newArrayT(1, newBasicT("int")),
							Elements: []ast.Expression{},
						},
					},
				},
			),
		},
		// var x [2][2][2]int = [2][2][2]int{}
		{
			decl: newVar(
				newID("x"),
				newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
				&ast.ArrayLiteral{
					TypeID:   newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
					Elements: []ast.Expression{},
				},
			),
		},
		// var x [2][2][2]int = [2][2][2]int{[2][2]int{}}
		{
			decl: newVar(
				newID("x"),
				newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
				&ast.ArrayLiteral{
					TypeID: newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
					Elements: []ast.Expression{
						&ast.ArrayLiteral{
							TypeID:   newArrayT(2, newArrayT(2, newBasicT("int"))),
							Elements: []ast.Expression{},
						},
					},
				},
			),
		},
		// var x [2][2][2]int = [2][2][2]int{
		//   [2][2]int{},
		//   [2][2]int{},
		// }
		{
			decl: newVar(
				newID("x"),
				newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
				&ast.ArrayLiteral{
					TypeID: newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
					Elements: []ast.Expression{
						&ast.ArrayLiteral{
							TypeID:   newArrayT(2, newArrayT(2, newBasicT("int"))),
							Elements: []ast.Expression{},
						},
						&ast.ArrayLiteral{
							TypeID:   newArrayT(2, newArrayT(2, newBasicT("int"))),
							Elements: []ast.Expression{},
						},
					},
				},
			),
		},
		// var x [2][2][2]int = [2][2][2]int{
		//   [2][2]int{ [2]int{1,2} },
		//   [2][2]int{}
		// }
		{
			decl: newVar(
				newID("x"),
				newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
				&ast.ArrayLiteral{
					TypeID: newArrayT(2, newArrayT(2, newArrayT(2, newBasicT("int")))),
					Elements: []ast.Expression{
						&ast.ArrayLiteral{
							TypeID: newArrayT(2, newArrayT(2, newBasicT("int"))),
							Elements: []ast.Expression{
								&ast.ArrayLiteral{
									TypeID: newArrayT(2, newBasicT("int")),
									Elements: []ast.Expression{
										newBasicV(1), newBasicV(2),
									},
								},
							},
						},
						&ast.ArrayLiteral{
							TypeID:   newArrayT(2, newArrayT(2, newBasicT("int"))),
							Elements: []ast.Expression{},
						},
					},
				},
			),
		},
	}

	for i, tc := range cases {
		tc := tc
		t.Run(fmt.Sprintf("case-%d", i), func(t *testing.T) {
			s := newSymbols()
			s.register(tc.decl)

			err := Analyze(newFile(tc.decl), s)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			arrT, ok := tc.decl.Expression.Type().(Array)
			if !ok {
				t.Fatalf("type %T not an array, go '%v'", tc.decl.Expression.Type(), tc.decl.Expression.Type())
			}

			if !arrT.Equals(tc.decl.Type()) {
				t.Fatalf("type of var decl '%v' does not match expressions type '%v'", tc.decl.Type(), arrT)
			}
		})
	}
}

func TestAnalyze_PrefixExpression(t *testing.T) {
	mainF := newFunc("main", withStatements(
		newExprStmt(&ast.PrefixExpression{
			Operator: "-",
			Right:    newBasicV(false),
		}),
	))

	s := newSymbols()
	s.register(mainF)

	f := newFile(mainF)

	expectErr := newTypeError(prefixErrOperatorIncompatible)

	err := Analyze(f, s)
	if err == nil {
		t.Fatalf("expecting error, got nil")
	}
	if !errors.Is(err, expectErr) {
		t.Fatalf("expecting error '%v', got '%v'", expectErr, err)
	}
}

func TestAnalyze_InfixExpression(t *testing.T) {
	cases := []struct {
		name   string
		expr   *ast.InfixExpression
		expect error
	}{
		{
			name: "type mismatch",
			expr: &ast.InfixExpression{
				Operator: "+",
				Left:     newBasicV(1),
				Right:    newBasicV(false),
			},
			expect: newTypeError(infixErrOperandMismatch).
				WithExpect(NewInt()).
				WithActual(NewBool()),
		},
		{
			name: "bad operator",
			expr: &ast.InfixExpression{
				Operator: "+",
				Left:     newBasicV(true),
				Right:    newBasicV(false),
			},
			expect: newTypeError(infixErrOperatorIncompatible),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			mainF := newFunc("main", withStatements(newExprStmt(tc.expr)))

			s := newSymbols()
			s.register(mainF)

			f := newFile(mainF)

			err := Analyze(f, s)
			if err == nil {
				t.Fatalf("expecting error, got nil")
			}
			if !errors.Is(err, tc.expect) {
				t.Fatalf("expecting error '%v', got '%v'", tc.expect, err)
			}
		})
	}
}

func TestAnalyze_IndexExpression(t *testing.T) {
	cases := []struct {
		name   string
		expr   *ast.IndexExpression
		expect error
	}{
		{
			name: "not indexable",
			expr: &ast.IndexExpression{
				Left:  newBasicV(1),
				Index: newBasicV(1),
			},
			expect: newTypeError(indexExprErrNotArray),
		},
		{
			name: "invalid index type",
			expr: &ast.IndexExpression{
				Left: &ast.ArrayLiteral{
					Elements: nil,
					TypeID:   newArrayT(1, newBasicT("int")),
					T:        nil,
				},
				Index: newBasicV(false),
			},
			expect: newTypeError(indexExprErrInvalidIndex).
				WithExpect(NewInt()).
				WithActual(NewBool()),
		},
	}
	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			mainF := newFunc("main", withStatements(newExprStmt(tc.expr)))

			s := newSymbols()
			s.register(mainF)

			f := newFile(mainF)

			err := Analyze(f, s)
			if err == nil {
				t.Fatalf("expecting error, got nil")
			}
			if !errors.Is(err, tc.expect) {
				t.Fatalf("expecting error '%v', got '%v'", tc.expect, err)
			}
		})
	}
}

func TestAnalyze_CallExpression(t *testing.T) {
	f1 := newFunc("f1")
	f1.T = NewFunction(NewVoid(), nil)
	f2 := newFunc("f2", withParam("x", newBasicT("int")))
	f2.T = NewFunction(NewVoid(), []ast.Type{NewInt()})

	cases := []struct {
		name   string
		expr   *ast.CallExpression
		expect error
	}{
		{
			name: "too many args",
			expr: &ast.CallExpression{
				Function:  f1.Identifier,
				Arguments: []ast.Expression{newBasicV(1)},
			},
			expect: newTypeError(callExprErrInvalidNumOfArgument),
		},
		{
			name: "not enough args",
			expr: &ast.CallExpression{
				Function:  f2.Identifier,
				Arguments: []ast.Expression{},
			},
			expect: newTypeError(callExprErrInvalidNumOfArgument),
		},
		{
			name: "arg type mismatch",
			expr: &ast.CallExpression{
				Function:  f2.Identifier,
				Arguments: []ast.Expression{newBasicV(false)},
			},
			expect: newTypeError(fmt.Sprintf(callExprErrInvalidArgumentFmt, 1)).
				WithExpect(NewInt()).
				WithActual(NewBool()),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			mainF := newFunc("main", withStatements(newExprStmt(tc.expr)))

			s := newSymbols()
			s.register(mainF)
			s.register(f1)
			s.register(f2)

			f := newFile(mainF)

			err := Analyze(f, s)
			if err == nil {
				t.Fatalf("expecting error, got nil")
			}
			if !errors.Is(err, tc.expect) {
				t.Fatalf("expecting error '%v', got '%v'", tc.expect, err)
			}
		})
	}
}

func newFile(declarations ...ast.Declaration) *ast.File {
	return &ast.File{Declarations: declarations}
}

func newVar(id *ast.Identifier, t ast.TypeIdentifier, expression ast.Expression) *ast.VarDeclaration {
	return &ast.VarDeclaration{
		Identifier: id,
		TypeName:   t,
		Expression: expression,
	}
}

func newConst(id *ast.Identifier, t ast.TypeIdentifier, expression ast.ConstExpression) *ast.ConstDeclaration {
	return &ast.ConstDeclaration{
		Identifier: id,
		TypeName:   t,
		Expression: expression,
	}
}

func newID(id string) *ast.Identifier {
	return &ast.Identifier{Name: id}
}
