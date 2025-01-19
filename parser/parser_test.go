package parser

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	"github.com/c0depwn/tinylang/pkg/ext"
	"strings"
	"testing"
)

// TODO: improvement: test error reporting in case of bad input
//		 current tests pretty much only check the happy path

// -- Helpers Statements

func createAssertVarDecl[E ast.Expression](
	assertI func(*testing.T, *ast.Identifier),
	assertT func(*testing.T, ast.TypeIdentifier),
	assertE func(*testing.T, E),
) func(*testing.T, *ast.VarDeclaration) {
	return func(t *testing.T, declaration *ast.VarDeclaration) {
		assertI(t, declaration.Identifier)
		assertT(t, declaration.TypeName)

		if assertE == nil {
			if declaration.Expression != nil {
				t.Fatalf("expected expression to be nil, got %v", declaration.Expression)
			}
		} else {
			assertedE, ok := (declaration.Expression).(E)
			if !ok {
				t.Fatalf("expected expression to satifsy %T, got %T", new(E), declaration.Expression)
			}
			assertE(t, assertedE)
		}
	}
}

func createAssertConstDecl[E ast.ConstExpression](
	assertI func(*testing.T, *ast.Identifier),
	assertT func(*testing.T, ast.TypeIdentifier),
	assertE func(*testing.T, E),
) func(*testing.T, *ast.ConstDeclaration) {
	return func(t *testing.T, declaration *ast.ConstDeclaration) {
		if assertE == nil {
			t.Fatalf("bad test: assertE required: const declaration must contain an initialization expression")
		}

		assertI(t, declaration.Identifier)
		assertT(t, declaration.TypeName)

		assertedE, ok := (declaration.Expression).(E)
		if !ok {
			t.Fatalf("expected expression to satifsy %T, got %T", new(E), declaration.Expression)
		}
		assertE(t, assertedE)
	}
}

func createAssertFuncDecl(
	assertI func(*testing.T, *ast.Identifier),
	assertResultT func(*testing.T, ast.TypeIdentifier),
	assertBlock func(*testing.T, *ast.Block),
	assertParams ...func(*testing.T, *ast.Param),
) func(*testing.T, *ast.FuncDeclaration) {
	return func(t *testing.T, declaration *ast.FuncDeclaration) {
		assertI(t, declaration.Identifier)
		assertBlock(t, declaration.Body)

		if len(assertParams) != len(declaration.Parameters) {
			t.Fatalf(fmt.Sprintf("expected %d parameters, got %d", len(assertParams), len(declaration.Parameters)))
		}

		for i, param := range assertParams {
			param(t, declaration.Parameters[i])
		}

		if assertResultT == nil {
			if declaration.Result != nil {
				t.Fatalf("expected result to be nil, got '%v'", declaration.Result)
			}
		} else {
			assertResultT(t, declaration.Result)
		}
	}
}

func createAssertAssignment(
	assertL func(*testing.T, ast.Expression),
	assertR func(*testing.T, ast.Expression),
) func(*testing.T, *ast.Assignment) {
	return func(t *testing.T, assignment *ast.Assignment) {
		assertL(t, assignment.Left)
		assertR(t, assignment.Value)
	}
}

func createAssertIf(
	assertCondition func(*testing.T, ast.Expression),
	assertConsequence func(*testing.T, *ast.Block),
	assertAlternative func(*testing.T, ast.Statement),
) func(*testing.T, *ast.IfStatement) {
	return func(t *testing.T, ifStatement *ast.IfStatement) {
		assertCondition(t, ifStatement.Condition)
		assertConsequence(t, ifStatement.Consequence)

		if assertAlternative == nil {
			if ifStatement.Alternative != nil {
				t.Fatalf("expected alternative to be nil, got %v", ifStatement.Alternative)
			}
		} else {
			assertAlternative(t, ifStatement.Alternative)
		}
	}
}

func createAssertWhile(
	assertE func(*testing.T, ast.Expression),
	assertB func(*testing.T, *ast.Block),
) func(*testing.T, *ast.WhileStatement) {
	return func(t *testing.T, whileStatement *ast.WhileStatement) {
		assertE(t, whileStatement.Condition)
		assertB(t, whileStatement.Body)
	}
}

func createAssertReturn(
	assertE func(*testing.T, ast.Expression),
) func(*testing.T, *ast.ReturnStatement) {
	return func(t *testing.T, argument *ast.ReturnStatement) {
		if assertE == nil {
			if argument.Expression != nil {
				t.Fatalf("expected expression to be nil, got %v", argument.Expression)
			}
		} else {
			assertE(t, argument.Expression)
		}
	}
}

// -- Helpers Expressions

func createAssertType(expectedType string) func(*testing.T, ast.TypeIdentifier) {
	return func(t *testing.T, identifier ast.TypeIdentifier) {
		switch typeID := identifier.(type) {
		case *ast.ArrayType:
			if typeID.String() != expectedType {
				t.Fatalf("expected '%s', got array type '%s'", expectedType, typeID.String())
			}
		case *ast.BasicTypeName:
			if typeID.String() != expectedType {
				t.Fatalf("expected '%s', got identifier '%s'", expectedType, typeID.String())
			}
		default:
			t.Fatalf("expected ast.TypeIdentifier, got %T", identifier)
		}
	}
}

func createAssertIdentifier(expectedName string) func(*testing.T, *ast.Identifier) {
	return func(t *testing.T, identifier *ast.Identifier) {
		if identifier.Name != expectedName {
			t.Fatalf("expected identifier '%s', got '%s'", expectedName, identifier.Name)
		}
	}
}

func createAssertArrayLiteral(
	expectTypeID func(*testing.T, ast.TypeIdentifier),
	expectElements ...func(*testing.T, ast.Expression),
) func(*testing.T, *ast.ArrayLiteral) {
	return func(t *testing.T, literal *ast.ArrayLiteral) {
		if len(literal.Elements) != len(expectElements) {
			t.Fatalf("expected %d elements, got %d", len(expectElements), len(literal.Elements))
		}

		expectTypeID(t, literal.TypeID)

		for i, elementAssertF := range expectElements {
			elementAssertF(t, literal.Elements[i])
		}
	}
}

func createAssertPrefixExpression(
	op string,
	assertR func(*testing.T, ast.Expression),
) func(*testing.T, *ast.PrefixExpression) {
	return func(t *testing.T, expression *ast.PrefixExpression) {
		if expression.Operator != op {
			t.Fatalf("expected operator '%s', got '%s'", op, expression.Operator)
		}
		if assertR == nil {
			t.Fatalf("bad test: assertR required: prefix expression must contain an expression")
		}
		assertR(t, expression.Right)
	}
}

func createAssertInfixExpression(
	operator string,
	assertL func(*testing.T, ast.Expression),
	assertR func(*testing.T, ast.Expression),
) func(*testing.T, *ast.InfixExpression) {
	return func(t *testing.T, expression *ast.InfixExpression) {
		if expression.Operator != operator {
			t.Fatalf("expected operator '%s', got '%s'", operator, expression.Operator)
		}

		assertL(t, expression.Left)
		assertR(t, expression.Right)
	}
}

func createAssertIndexExpression(
	assertLeft func(*testing.T, ast.Expression),
	assertIndex func(*testing.T, ast.Expression),
) func(*testing.T, *ast.IndexExpression) {
	return func(t *testing.T, expression *ast.IndexExpression) {
		assertLeft(t, expression.Left)
		assertIndex(t, expression.Index)
	}
}

func createAssertCallExpr(
	fName string,
	assertArgs ...func(*testing.T, ast.Expression),
) func(*testing.T, *ast.CallExpression) {
	return func(t *testing.T, expression *ast.CallExpression) {
		id := expression.Function
		if id.Name != fName {
			t.Fatalf("expected identifier '%s', got '%s'", fName, id.Name)
		}
		if len(assertArgs) != len(expression.Arguments) {
			t.Fatalf("expected %d arguments, got %d", len(assertArgs), len(expression.Arguments))
		}
		for i, arg := range assertArgs {
			arg(t, expression.Arguments[i])
		}
	}
}

func createAssertBasicLiteral[T comparable](expect T) func(*testing.T, *ast.BasicLiteral) {
	return func(t *testing.T, lit *ast.BasicLiteral) {
		actual, ok := constant.As[T](lit.Value())
		if !ok {
			t.Fatalf("could not convert constant value to %T", expect)
		}
		if expect != actual {
			t.Fatalf("expected '%v', got '%v'", expect, actual)
		}
	}
}

func createAssertBlock(statements ...func(*testing.T, ast.Statement)) func(*testing.T, *ast.Block) {
	return func(t *testing.T, block *ast.Block) {
		if len(statements) != len(block.Statements) {
			t.Fatalf(fmt.Sprintf("expected %d statements, got %d", len(statements), len(block.Statements)))
		}
		for i, statement := range statements {
			statement(t, block.Statements[i])
		}
	}
}

func createAssertParam(
	name string,
	assertT func(*testing.T, ast.TypeIdentifier),
) func(*testing.T, *ast.Param) {
	return func(t *testing.T, param *ast.Param) {
		if name != param.Identifier.Name {
			t.Fatalf(fmt.Sprintf("expected param '%s', got '%s'", name, param.Identifier.Name))
		}
		if assertT == nil {
			t.Fatalf(fmt.Sprintf("bad test: assertT in param assertion is required"))
		}

		assertT(t, param.TypeID)
	}
}

// lift an assertion function of a specific expression up to func(*testing.T, ast.Expression),
// so that it can be used in mustParsePartial.
func lift[T ast.Expression](f func(*testing.T, T)) func(*testing.T, ast.Expression) {
	return func(t *testing.T, expression ast.Expression) {
		// The type assertion must always succeed as the constraint of lift
		// requires that ast.Expression is implemented by the generic argument of f.
		e, ok := expression.(T)
		if !ok {
			panic(fmt.Sprintf("expected expression to satifsy %T, got %T", new(T), expression))
		}

		f(t, e)
	}
}

func liftStmt[T ast.Statement](f func(*testing.T, T)) func(*testing.T, ast.Statement) {
	return func(t *testing.T, statement ast.Statement) {
		// The type assertion must always succeed as the constraint of lift
		// requires that ast.Statement is implemented by the generic argument of f.
		s, ok := statement.(T)
		if !ok {
			panic(fmt.Sprintf("expected expression to satifsy %T, got %T", new(T), s))
		}

		f(t, s)
	}
}

// --- Statements

// TestVarDeclaration ensures that various forms of variable declarations are being parsed as expected.
// Testing complex expression is out of scope for this test.
func TestParser_VarDeclaration(t *testing.T) {
	cases := []struct {
		name   string
		input  string
		assert func(*testing.T, *ast.VarDeclaration)
	}{
		{
			name:  "int decimal literal",
			input: "var x int = 1;",
			assert: createAssertVarDecl(
				createAssertIdentifier("x"),
				createAssertType("int"),
				createAssertBasicLiteral(int64(1)),
			),
		},
		{
			name:  "int binary literal",
			input: "var x int = 0b10;",
			assert: createAssertVarDecl(
				createAssertIdentifier("x"),
				createAssertType("int"),
				createAssertBasicLiteral(int64(0b10)),
			),
		},
		{
			name:  "int hex lower case literal",
			input: "var x int = 0xff;",
			assert: createAssertVarDecl(
				createAssertIdentifier("x"),
				createAssertType("int"),
				createAssertBasicLiteral(int64(0xff)),
			),
		},
		{
			name:  "int hex upper case literal",
			input: "var x int = 0x10AAFF;",
			assert: createAssertVarDecl(
				createAssertIdentifier("x"),
				createAssertType("int"),
				createAssertBasicLiteral(int64(0x10AAFF)),
			),
		},
		{
			name:  "without initialization",
			input: "var x int;",
			assert: createAssertVarDecl[ast.Expression](
				createAssertIdentifier("x"),
				createAssertType("int"),
				nil,
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			p := strParser(tc.input)

			mustParsePartial(
				t,
				p.parseVarDeclaration,
				tc.assert,
			)
		})
	}
}

func TestParser_ConstDeclaration(t *testing.T) {
	cases := []struct {
		name   string
		input  string
		assert func(*testing.T, *ast.ConstDeclaration)
	}{
		{
			name:  "simple const",
			input: "const x int = 1",
			assert: createAssertConstDecl(
				createAssertIdentifier("x"),
				createAssertType("int"),
				createAssertBasicLiteral(int64(1)),
			),
		},
		{
			name:  "simple const",
			input: "const x bool = true",
			assert: createAssertConstDecl(
				createAssertIdentifier("x"),
				createAssertType("bool"),
				createAssertBasicLiteral(true),
			),
		},
		{
			name:  "simple const",
			input: "const x string = `hello, world!`",
			assert: createAssertConstDecl(
				createAssertIdentifier("x"),
				createAssertType("string"),
				createAssertBasicLiteral("hello, world!"),
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parseConstDeclaration, tc.assert)
		})
	}
}

func TestParser_FunctionDeclaration(t *testing.T) {
	cases := []struct {
		name   string
		input  string
		assert func(*testing.T, *ast.FuncDeclaration)
	}{
		{
			name:  "basic func",
			input: "fn myFunc() {}",
			assert: createAssertFuncDecl(
				createAssertIdentifier("myFunc"),
				nil,
				createAssertBlock(),
			),
		},
		{
			name:  "func with params",
			input: "fn myFunc(a int, b bool) {}",
			assert: createAssertFuncDecl(
				createAssertIdentifier("myFunc"),
				nil,
				createAssertBlock(),
				createAssertParam("a", createAssertType("int")),
				createAssertParam("b", createAssertType("bool")),
			),
		},
		{
			name:  "func with result",
			input: "fn myFunc() int {}",
			assert: createAssertFuncDecl(
				createAssertIdentifier("myFunc"),
				createAssertType("int"),
				createAssertBlock(),
			),
		},
		{
			name:  "func with result",
			input: "fn length(a string) int {}",
			assert: createAssertFuncDecl(
				createAssertIdentifier("length"),
				createAssertType("int"),
				createAssertBlock(),
				createAssertParam("a", createAssertType("string")),
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parseFunctionDeclaration, tc.assert)
		})
	}
}

func TestParser_Assignment(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.Assignment)
	}{
		{
			input: "a = 1",
			assert: createAssertAssignment(
				lift(createAssertIdentifier("a")),
				lift(createAssertBasicLiteral(int64(1))),
			),
		},
		{
			input: "a = 1 + 1",
			assert: createAssertAssignment(
				lift(createAssertIdentifier("a")),
				lift(createAssertInfixExpression(
					"+",
					lift(createAssertBasicLiteral(int64(1))),
					lift(createAssertBasicLiteral(int64(1))),
				)),
			),
		},
		{
			input: "a = [1]int{}",
			assert: createAssertAssignment(
				lift(createAssertIdentifier("a")),
				lift(createAssertArrayLiteral(createAssertType("[1]int"))),
			),
		},
		{
			input: "a = b",
			assert: createAssertAssignment(
				lift(createAssertIdentifier("a")),
				lift(createAssertIdentifier("b")),
			),
		},
		{
			input: "a = f()",
			assert: createAssertAssignment(
				lift(createAssertIdentifier("a")),
				lift(createAssertCallExpr("f")),
			),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, func() *ast.Assignment {
				e := p.parseExpression(Lowest)
				p.advance()
				return p.parseAssignment(e)
			}, tc.assert)
		})
	}
}

func TestParser_CompoundAssignment(t *testing.T) {
	// The assigned expressions are always the same in this test therefore, the assert
	// function can be re-used by swapping out the operator.
	// The defined test cases focus on the assignment operator rather than the expression.
	createAssert := func(op string) func(*testing.T, *ast.Assignment) {
		return createAssertAssignment(
			lift(createAssertIdentifier("x")),
			lift(createAssertInfixExpression(
				op,
				lift(createAssertIdentifier("x")),
				lift(createAssertInfixExpression(
					"+",
					lift(createAssertBasicLiteral(int64(1))),
					lift(createAssertInfixExpression(
						"*",
						lift(createAssertBasicLiteral(int64(2))),
						lift(createAssertBasicLiteral(int64(3))),
					)),
				)),
			)),
		)
	}

	cases := []struct {
		input  string
		expect string
		assert func(*testing.T, *ast.Assignment)
	}{
		{
			input:  "x *= 1 + 2 * 3",
			expect: "x = (x * (1 + (2 * 3)))",
			assert: createAssert("*"),
		},
		{
			input:  "x /= 1 + 2 * 3",
			expect: "x = (x / (1 + (2 * 3)))",
			assert: createAssert("/"),
		},
		{
			input:  "x %= 1 + 2 * 3",
			expect: "x = (x % (1 + (2 * 3)))",
			assert: createAssert("%"),
		},
		{
			input:  "x += 1 + 2 * 3",
			expect: "x = (x + (1 + (2 * 3)))",
			assert: createAssert("+"),
		},
		{
			input:  "x -= 1 + 2 * 3",
			expect: "x = (x - (1 + (2 * 3)))",
			assert: createAssert("-"),
		},
		{
			input:  "x <<= 1 + 2 * 3",
			expect: "x = (x << (1 + (2 * 3)))",
			assert: createAssert("<<"),
		},
		{
			input:  "x >>= 1 + 2 * 3",
			expect: "x = (x >> (1 + (2 * 3)))",
			assert: createAssert(">>"),
		},
		{
			input:  "x &= 1 + 2 * 3",
			expect: "x = (x & (1 + (2 * 3)))",
			assert: createAssert("&"),
		},
		{
			input:  "x |= 1 + 2 * 3",
			expect: "x = (x | (1 + (2 * 3)))",
			assert: createAssert("|"),
		},
		{
			input:  "x ^= 1 + 2 * 3",
			expect: "x = (x ^ (1 + (2 * 3)))",
			assert: createAssert("^"),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)

			var node *ast.Assignment

			parseF := func() *ast.Assignment {
				e := p.parseExpression(Lowest)
				p.advance()
				node = p.parseAssignment(e)
				return node
			}

			// assert ast
			mustParsePartial(t, parseF, tc.assert)

			// ensure generated string matches expectation
			if node.String() != tc.expect {
				t.Fatalf("expected '%s', got '%s'", tc.expect, node.String())
			}
		})
	}
}

func TestParser_IfStatement(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.IfStatement)
	}{
		{
			input: "if x {}",
			assert: createAssertIf(
				lift(createAssertIdentifier("x")),
				createAssertBlock(),
				nil, // no else block
			),
		},
		{
			input: "if x {} else {}",
			assert: createAssertIf(
				lift(createAssertIdentifier("x")),
				createAssertBlock(),
				liftStmt(createAssertBlock()),
			),
		},
		{
			input: "if x {} else if y {}",
			assert: createAssertIf(
				lift(createAssertIdentifier("x")),
				createAssertBlock(),
				liftStmt(createAssertIf(
					lift(createAssertIdentifier("y")),
					createAssertBlock(),
					nil, // no else block
				)),
			),
		},
		{
			input: "if x {} else if y {} else {}",
			assert: createAssertIf(
				lift(createAssertIdentifier("x")),
				createAssertBlock(),
				liftStmt(createAssertIf(
					lift(createAssertIdentifier("y")),
					createAssertBlock(),
					liftStmt(createAssertBlock()),
				)),
			),
		},
		{
			input: "if true || x {} else if false && y {} else {}",
			assert: createAssertIf(
				lift(createAssertInfixExpression(
					"||",
					lift(createAssertBasicLiteral(true)),
					lift(createAssertIdentifier("x")),
				)),
				createAssertBlock(),
				liftStmt(createAssertIf(
					lift(createAssertInfixExpression(
						"&&",
						lift(createAssertBasicLiteral(false)),
						lift(createAssertIdentifier("y")),
					)),
					createAssertBlock(),
					liftStmt(createAssertBlock()),
				)),
			),
		},
	}
	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parseIfStatement, tc.assert)
		})
	}
}

func TestParser_WhileStatement(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.WhileStatement)
	}{
		{
			input: "while x {}",
			assert: createAssertWhile(
				lift(createAssertIdentifier("x")),
				createAssertBlock(),
			),
		},
		{
			input: "while true && false {}",
			assert: createAssertWhile(
				lift(createAssertInfixExpression(
					"&&",
					lift(createAssertBasicLiteral(true)),
					lift(createAssertBasicLiteral(false)),
				)),
				createAssertBlock(),
			),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parseWhileStatement, tc.assert)
		})
	}
}

func TestParser_ForStatement(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.Block)
	}{
		{
			input: "for ;x; {}",
			assert: createAssertBlock(
				liftStmt(createAssertWhile(
					lift(createAssertIdentifier("x")),
					createAssertBlock(),
				)),
			),
		},
		{
			input: "for var x int = 1; true; x += 1 {}",
			assert: createAssertBlock(
				liftStmt(createAssertVarDecl(
					createAssertIdentifier("x"),
					createAssertType("int"),
					createAssertBasicLiteral(int64(1)),
				)),
				liftStmt(createAssertWhile(
					lift(createAssertBasicLiteral(true)),
					createAssertBlock(
						liftStmt(createAssertAssignment(
							lift(createAssertIdentifier("x")),
							lift(createAssertInfixExpression(
								"+",
								lift(createAssertIdentifier("x")),
								lift(createAssertBasicLiteral(int64(1))),
							)),
						)),
					),
				)),
			),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, func() ast.Statement {
				return convertToWhile(p.parseForStatement())
			}, liftStmt(tc.assert))
		})
	}
}

func TestParser_ReturnStatement(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.ReturnStatement)
	}{
		{
			input:  "return;",
			assert: createAssertReturn(nil),
		},
		{
			input:  "return true;",
			assert: createAssertReturn(lift(createAssertBasicLiteral(true))),
		},
		{
			input: "return a + 1;",
			assert: createAssertReturn(
				lift(createAssertInfixExpression(
					"+",
					lift(createAssertIdentifier("a")),
					lift(createAssertBasicLiteral(int64(1))),
				)),
			),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parseReturnStatement, tc.assert)
		})
	}
}

// --- Expressions

// TestIntLiteral verifies that integer literals are parsed correctly.
// This includes integer literals in decimal, binary and hexadecimal form.
// TODO: improvement: what happens when we overflow/underflow?
func TestParser_IntLiteral(t *testing.T) {
	cases := []struct {
		input      string
		expect     int64
		assertFunc func(*testing.T, *ast.BasicLiteral)
	}{
		{
			input:      "999",
			expect:     999,
			assertFunc: createAssertBasicLiteral[int64](999),
		},
		{
			input:      "0b1010101",
			expect:     0b1010101,
			assertFunc: createAssertBasicLiteral[int64](0b1010101),
		},
		{
			input:      "0xdeadbeef",
			expect:     0xdeadbeef,
			assertFunc: createAssertBasicLiteral[int64](0xdeadbeef),
		},
		{
			input:      "0xDEADBEEF",
			expect:     0xDEADBEEF,
			assertFunc: createAssertBasicLiteral[int64](0xDEADBEEF),
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()

			p := strParser(tc.input)

			mustParsePartial(
				t,
				p.parseBasicLiteral,
				lift(tc.assertFunc),
			)
		})
	}
}

// TestParser_BoolLiteral verifies that boolean literals are parsed correctly.
func TestParser_BoolLiteral(t *testing.T) {
	cases := []struct {
		input  string
		expect bool
	}{
		{
			input:  "true",
			expect: true,
		},
		{
			input:  "false",
			expect: false,
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()

			p := strParser(tc.input)

			assertFunc := createAssertBasicLiteral[bool](tc.expect)

			mustParsePartial(
				t,
				p.parseBasicLiteral,
				lift(assertFunc),
			)
		})
	}
}

// TestParser_StringLiteral verifies that string literals are parsed correctly.
func TestParser_StringLiteral(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.BasicLiteral)
	}{
		{
			input:  "`asdf`",
			assert: createAssertBasicLiteral("asdf"),
		},
		{
			input:  "``",
			assert: createAssertBasicLiteral(""),
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()

			p := strParser(tc.input)

			mustParsePartial(t, p.parseBasicLiteral, lift(tc.assert))
		})
	}
}

// TestParser_ArrayLiteral verifies that array literals are parsed correctly.
func TestParser_ArrayLiteral(t *testing.T) {
	cases := []struct {
		input      string
		expect     string
		assertFunc func(*testing.T, *ast.ArrayLiteral)
	}{
		{
			input:      "[1]int{}",
			assertFunc: createAssertArrayLiteral(createAssertType("[1]int")),
		},
		{
			input: "[3]int{1,2,3}",
			assertFunc: createAssertArrayLiteral(
				createAssertType("[3]int"),
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
				lift(createAssertBasicLiteral(int64(3))),
			),
		},
		{
			input: "[2]string{`a`,`b`}",
			assertFunc: createAssertArrayLiteral(
				createAssertType("[2]string"),
				lift(createAssertBasicLiteral("a")),
				lift(createAssertBasicLiteral("b")),
			),
		},
		{
			input: "[1][1]int{[1]int{},[1]int{}}",
			assertFunc: createAssertArrayLiteral(
				createAssertType("[1][1]int"),
				lift(createAssertArrayLiteral(createAssertType("[1]int"))),
				lift(createAssertArrayLiteral(createAssertType("[1]int"))),
			),
		},
		{
			input: "[2][2]bool{[2]bool{true, false},[2]bool{true, false}}",
			assertFunc: createAssertArrayLiteral(
				createAssertType("[2][2]bool"),
				lift(createAssertArrayLiteral(
					createAssertType("[2]bool"),
					lift(createAssertBasicLiteral(true)),
					lift(createAssertBasicLiteral(false)),
				)),
				lift(createAssertArrayLiteral(
					createAssertType("[2]bool"),
					lift(createAssertBasicLiteral(true)),
					lift(createAssertBasicLiteral(false)),
				)),
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parseArrayLiteral, lift(tc.assertFunc))
		})
	}
}

func TestParser_PrefixExpression(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.PrefixExpression)
	}{
		{
			input:  "-2",
			assert: createAssertPrefixExpression("-", lift(createAssertBasicLiteral(int64(2)))),
		},
		{
			// This is equivalent to -(-(2)) because there is no "--" decrement operator.
			input: "--2",
			assert: createAssertPrefixExpression(
				"-", lift(createAssertPrefixExpression(
					"-", lift(createAssertBasicLiteral(int64(2))),
				)),
			),
		},
		{
			input:  "!true",
			assert: createAssertPrefixExpression("!", lift(createAssertBasicLiteral(true))),
		},
		{
			input: "!!false",
			assert: createAssertPrefixExpression(
				"!", lift(createAssertPrefixExpression(
					"!", lift(createAssertBasicLiteral(false)),
				)),
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(t, p.parsePrefixExpression, lift(tc.assert))
		})
	}

}

func TestParser_InfixExpression(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.InfixExpression)
	}{
		// arithmetic
		{
			input: "1 * 2",
			assert: createAssertInfixExpression(
				"*",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 / 2",
			assert: createAssertInfixExpression(
				"/",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 % 2",
			assert: createAssertInfixExpression(
				"%",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 + 2",
			assert: createAssertInfixExpression(
				"+",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 - 2",
			assert: createAssertInfixExpression(
				"-",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},

		// shifts
		{
			input: "1 << 2",
			assert: createAssertInfixExpression(
				"<<",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 >> 2",
			assert: createAssertInfixExpression(
				">>",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		// bitwise ops
		{
			input: "1 ^ 2",
			assert: createAssertInfixExpression(
				"^",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 | 2",
			assert: createAssertInfixExpression(
				"|",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "2 & 1",
			assert: createAssertInfixExpression(
				"&",
				lift(createAssertBasicLiteral(int64(2))),
				lift(createAssertBasicLiteral(int64(1))),
			),
		},
		// equality
		{
			input: "1 == 2",
			assert: createAssertInfixExpression(
				"==",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 != 2",
			assert: createAssertInfixExpression(
				"!=",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 <= 2",
			assert: createAssertInfixExpression(
				"<=",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		{
			input: "1 >= 2",
			assert: createAssertInfixExpression(
				">=",
				lift(createAssertBasicLiteral(int64(1))),
				lift(createAssertBasicLiteral(int64(2))),
			),
		},
		// logic
		{
			input: "true || true",
			assert: createAssertInfixExpression(
				"||",
				lift(createAssertBasicLiteral(true)),
				lift(createAssertBasicLiteral(true)),
			),
		},
		{
			input: "false && false",
			assert: createAssertInfixExpression(
				"&&",
				lift(createAssertBasicLiteral(false)),
				lift(createAssertBasicLiteral(false)),
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)
			mustParsePartial(
				t,
				func() ast.Expression { return p.parseExpression(Lowest) },
				lift(tc.assert),
			)
		})
	}
}

func TestParser_ArrayIndexExpression(t *testing.T) {
	cases := []struct {
		input  string
		assert func(*testing.T, *ast.IndexExpression)
	}{
		{
			input: "myArray[0]",
			assert: createAssertIndexExpression(
				lift(createAssertIdentifier("myArray")),
				lift(createAssertBasicLiteral(int64(0))),
			),
		},
		{
			input: "myArray[3 * a]",
			assert: createAssertIndexExpression(
				lift(createAssertIdentifier("myArray")),
				lift(createAssertInfixExpression(
					"*",
					lift(createAssertBasicLiteral(int64(3))),
					lift(createAssertIdentifier("a")),
				)),
			),
		},
		{
			input: "f()[0]",
			assert: createAssertIndexExpression(
				lift(createAssertCallExpr("f")),
				lift(createAssertBasicLiteral(int64(0))),
			),
		},
		{
			input: "arr[0][1]",
			assert: createAssertIndexExpression(
				lift(createAssertIndexExpression(
					lift(createAssertIdentifier("arr")),
					lift(createAssertBasicLiteral(int64(0))),
				)),
				lift(createAssertBasicLiteral(int64(1))),
			),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			p := strParser(tc.input)

			parseF := func() ast.Expression {
				return p.parseExpression(Lowest)
			}

			mustParsePartial(t, parseF, lift(tc.assert))
		})
	}
}

func TestParser_CallExpression(t *testing.T) {
	// Notice: f()() is not possible as returning functions is not yet supported
	cases := []struct {
		name   string
		input  string
		assert func(*testing.T, *ast.CallExpression)
	}{
		{
			name:   "basic call without args",
			input:  "a()",
			assert: createAssertCallExpr("a"),
		},
		{
			name:  "single param",
			input: "a(param1)",
			assert: createAssertCallExpr(
				"a",
				lift(createAssertIdentifier("param1")),
			),
		},
		{
			name:  "two params",
			input: "a(param1, param2)",
			assert: createAssertCallExpr(
				"a",
				lift(createAssertIdentifier("param1")),
				lift(createAssertIdentifier("param2")),
			),
		},
		{
			name:  "three params",
			input: "a(param1, param2, param3)",
			assert: createAssertCallExpr(
				"a",
				lift(createAssertIdentifier("param1")),
				lift(createAssertIdentifier("param2")),
				lift(createAssertIdentifier("param3")),
			),
		},
		{
			name:  "expression params",
			input: "a(1 + 2 * 3, 2 << 1, other)",
			assert: createAssertCallExpr(
				"a",
				lift(createAssertInfixExpression(
					"+",
					lift(createAssertBasicLiteral(int64(1))),
					lift(createAssertInfixExpression(
						"*",
						lift(createAssertBasicLiteral(int64(2))),
						lift(createAssertBasicLiteral(int64(3))),
					)),
				),
				),
				lift(createAssertInfixExpression(
					"<<",
					lift(createAssertBasicLiteral(int64(2))),
					lift(createAssertBasicLiteral(int64(1))),
				)),
				lift(createAssertIdentifier("other")),
			),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			p := strParser(tc.input)

			parseF := func() ast.Expression { return p.parseExpression(Lowest) }
			assertF := lift(tc.assert)

			mustParsePartial(t, parseF, assertF)
		})
	}
}

func TestParser_ComplexExpression(t *testing.T) {
	cases := []struct {
		name   string
		input  string
		expect string
		assert func(*testing.T, ast.Expression)
	}{
		{
			name:   "Unary > Product",
			input:  "-1 * 4 / 5",
			expect: "((-(1) * 4) / 5)",
			assert: lift(createAssertInfixExpression(
				"/",
				lift(createAssertInfixExpression(
					"*",
					lift(createAssertPrefixExpression(
						"-", lift(createAssertBasicLiteral(int64(1)))),
					),
					lift(createAssertBasicLiteral(int64(4))),
				)),
				lift(createAssertBasicLiteral(int64(5))),
			)),
		},
		{
			name:   "Product > Sum",
			input:  "1 + 2 * 3 + 4 / 5",
			expect: "((1 + (2 * 3)) + (4 / 5))",
			assert: lift(createAssertInfixExpression(
				"+",
				lift(createAssertInfixExpression(
					"+",
					lift(createAssertBasicLiteral(int64(1))),
					lift(createAssertInfixExpression(
						"*",
						lift(createAssertBasicLiteral(int64(2))),
						lift(createAssertBasicLiteral(int64(3))),
					)),
				)),
				lift(createAssertInfixExpression(
					"/",
					lift(createAssertBasicLiteral(int64(4))),
					lift(createAssertBasicLiteral(int64(5))),
				)),
			)),
		},
		{
			name:   "Sum > BitShift",
			input:  "1 >> 2 + 3 << 4 + 5",
			expect: "((1 >> (2 + 3)) << (4 + 5))",
			assert: lift(createAssertInfixExpression(
				"<<",
				lift(createAssertInfixExpression(
					">>",
					lift(createAssertBasicLiteral(int64(1))),
					lift(createAssertInfixExpression(
						"+",
						lift(createAssertBasicLiteral(int64(2))),
						lift(createAssertBasicLiteral(int64(3))),
					)),
				)),
				lift(createAssertInfixExpression(
					"+",
					lift(createAssertBasicLiteral(int64(4))),
					lift(createAssertBasicLiteral(int64(5))),
				)),
			)),
		},
		{
			name:   "BitShift > Bitwise",
			input:  "1 << 0b01 & 0b01 | 0b10 << 1",
			expect: "(((1 << 0b01) & 0b01) | (0b10 << 1))",
			assert: lift(createAssertInfixExpression(
				"|",
				lift(createAssertInfixExpression(
					"&",
					lift(createAssertInfixExpression(
						"<<",
						lift(createAssertBasicLiteral(int64(1))),
						lift(createAssertBasicLiteral(int64(0b01))),
					)),
					lift(createAssertBasicLiteral(int64(0b01))),
				)),
				lift(createAssertInfixExpression(
					"<<",
					lift(createAssertBasicLiteral(int64(0b10))),
					lift(createAssertBasicLiteral(int64(1))),
				)),
			)),
		},
		{
			name:   "Bitwise > Equality",
			input:  "1 & 1 == 2 ^ 1 != 1 | 2 <= 1",
			expect: "((((1 & 1) == (2 ^ 1)) != (1 | 2)) <= 1)",
			assert: lift(createAssertInfixExpression(
				"<=",
				lift(createAssertInfixExpression(
					"!=",
					lift(createAssertInfixExpression(
						"==",
						lift(createAssertInfixExpression(
							"&",
							lift(createAssertBasicLiteral(int64(1))),
							lift(createAssertBasicLiteral(int64(1))),
						)),
						lift(createAssertInfixExpression(
							"^",
							lift(createAssertBasicLiteral(int64(2))),
							lift(createAssertBasicLiteral(int64(1))),
						)),
					)),
					lift(createAssertInfixExpression(
						"|",
						lift(createAssertBasicLiteral(int64(1))),
						lift(createAssertBasicLiteral(int64(2))),
					)),
				)),
				lift(createAssertBasicLiteral(int64(1))),
			)),
		},
		{
			name:   "Bitwise > Equality",
			input:  "1 & 1 == 2 ^ 1 != 1 | 2 <= 1 >= 2",
			expect: "(((((1 & 1) == (2 ^ 1)) != (1 | 2)) <= 1) >= 2)",
			assert: lift(createAssertInfixExpression(
				">=",
				lift(createAssertInfixExpression(
					"<=",
					lift(createAssertInfixExpression(
						"!=",
						lift(createAssertInfixExpression(
							"==",
							lift(createAssertInfixExpression(
								"&",
								lift(createAssertBasicLiteral(int64(1))),
								lift(createAssertBasicLiteral(int64(1))),
							)),
							lift(createAssertInfixExpression(
								"^",
								lift(createAssertBasicLiteral(int64(2))),
								lift(createAssertBasicLiteral(int64(1))),
							)),
						)),
						lift(createAssertInfixExpression(
							"|",
							lift(createAssertBasicLiteral(int64(1))),
							lift(createAssertBasicLiteral(int64(2))),
						)),
					)),
					lift(createAssertBasicLiteral(int64(1))),
				)),
				lift(createAssertBasicLiteral(int64(2))),
			)),
		},
		{
			name:   "Equality > Logical",
			input:  "true && false == false || true",
			expect: "((true && (false == false)) || true)",
			assert: lift(createAssertInfixExpression(
				"||",
				lift(createAssertInfixExpression(
					"&&",
					lift(createAssertBasicLiteral(true)),
					lift(createAssertInfixExpression(
						"==",
						lift(createAssertBasicLiteral(false)),
						lift(createAssertBasicLiteral(false)),
					)),
				)),
				lift(createAssertBasicLiteral(true)),
			)),
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			p := strParser(tc.input)

			var node ast.Expression

			// wrap the parser func to capture the result
			parseF := func() ast.Expression {
				node = p.parseExpression(Lowest)
				return node
			}

			// ensure AST is correct
			mustParsePartial(t, parseF, tc.assert)

			// ensure generated string matches expectation
			stringer, ok := node.(fmt.Stringer)
			if !ok {
				t.Fatalf("node must implement fmt.Stringer")
			}
			if stringer.String() != tc.expect {
				t.Fatalf("expected '%s', got '%s'", tc.expect, stringer)
			}
		})
	}
}

// mustParsePartial provides a generic way to execute an internal function
// of the [parser] and assert the produced [ast.Node] using the supplied assertFunc.
// The generic T is used to ensure result/argument compatibility between the supplied
// the parserFunc and assertFunc.
// Since the [parser] might return interfaces of the ast package instead of concrete structs,
// it might be necessary to use lift to "lift" the concrete type to a compatible interface it implements
// so that compatibility between the parseFunc and assertFunc is guaranteed.
// Example:
//
//	mustParsePartial(
//		t,
//		// parseBasicLiteral returns an ast.Expression (concretely an *ast.BasicLiteral)
//		p.parseBasicLiteral,
//		// lift ensures that the assertFunc (which accepts an *ast.BasicLiteral)
//		// is transformed to accept an ast.Expression.
//		lift(tc.assertFunc),
//	)
func mustParsePartial[T any](
	t *testing.T,
	parserFunc func() T,
	assertFunc func(*testing.T, T),
) {
	var result T

	err := ext.CatchPanic(func() {
		result = parserFunc()
	})

	if err != nil {
		t.Fatalf("expected err to be nil, got '%v'", err)
	}

	assertFunc(t, result)
}

func strParser(str string) *parser {
	return newParser(StringParserSource(str), PanicErrHandler)
}

func StringParserSource(str string) TokenSource {
	return LexerTokenSource(strings.NewReader(str))
}
