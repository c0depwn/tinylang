package symbols

import (
	"errors"
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	"github.com/c0depwn/tinylang/token"
	"testing"
)

func newFile(decls ...ast.Declaration) *ast.File {
	return &ast.File{Declarations: decls}
}

type varOpt func(*ast.VarDeclaration)

func varWithExpr(expr ast.Expression) varOpt {
	return func(decl *ast.VarDeclaration) {
		decl.Expression = expr
	}
}

func newBasicT(id string) *ast.BasicTypeName {
	return &ast.BasicTypeName{Name: id}
}

func newVar(id string, t ast.TypeIdentifier, opts ...varOpt) *ast.VarDeclaration {
	varDecl := &ast.VarDeclaration{
		Identifier: &ast.Identifier{Name: id},
		TypeName:   t,
	}
	for _, o := range opts {
		o(varDecl)
	}
	return varDecl
}

func newConst(id string, t ast.TypeIdentifier, v any) *ast.ConstDeclaration {
	l := &ast.BasicLiteral{}
	l.SetValue(constant.Make(v))

	return &ast.ConstDeclaration{
		Identifier: &ast.Identifier{Name: id},
		TypeName:   t,
		Expression: l,
	}
}

func withStatements(stmts ...ast.Statement) fOpt {
	return func(decl *ast.FuncDeclaration) {
		decl.Body.Statements = stmts
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
			T:          nil,
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

func TestAnalyze(t *testing.T) {
	cases := []struct {
		f        *ast.File
		expect   error
		builtins []string
	}{
		{
			// var x int;
			// var x int;
			f: newFile(
				newVar("x", newBasicT(token.Int)),
				newVar("x", newBasicT(token.Int)),
			),
			expect: newSymbolErrorF(registerErrFmt, "x"),
		},
		{
			// var x int;
			// const x int = 1;
			f: newFile(
				newVar("x", newBasicT(token.Int)),
				newConst("x", newBasicT(token.Int), 1),
			),
			expect: newSymbolErrorF(registerErrFmt, "x"),
		},
		// TODO: bug: global variable expressions are not traversed during symbol pass
		//       this test fails because no error is created but we expect an error
		//{
		//	// var x int = y;
		//	f: newFile(
		//		newVar("x", newBasicT(token.Int), varWithExpr(&ast.Identifier{Name: "y"})),
		//	),
		//	expect: newSymbolErrorF(lookupErrFmt, "y"),
		//},
		{
			// var x int;
			// fn f(x int) {}
			f: newFile(
				newVar("x", newBasicT(token.Int)),
				newFunc("f", withParam("x", newBasicT(token.Int))),
			),
			expect: newSymbolErrorF(registerErrFmt, "x"),
		},
		{
			// fn f() { x = y; }
			f: newFile(newFunc("f", withStatements(&ast.Assignment{
				Left:  &ast.Identifier{Name: "x"},
				Value: &ast.Identifier{Name: "y"},
			}))),
			expect: newSymbolErrorF(lookupErrFmt, "x"),
		},
		{
			// var x int;
			// fn f() { var x int; }
			f: newFile(
				newVar("x", newBasicT(token.Int)),
				newFunc(
					"f",
					withParam("x", newBasicT(token.Int)),
					withStatements(
						newVar("x", newBasicT(token.Int)),
					),
				),
			),
			expect: newSymbolErrorF(registerErrFmt, "x"),
		},
		{
			// var f int;
			// fn f() {}
			f: newFile(
				newVar("f", newBasicT(token.Int)),
				newFunc("f"),
			),
			expect: newSymbolErrorF(registerErrFmt, "f"),
		},
		{
			// fn f() {}
			// fn f() {}
			f: newFile(
				newFunc("f"),
				newFunc("f"),
			),
			expect: newSymbolErrorF(registerErrFmt, "f"),
		},
		{
			// fn f() { myCustomBuiltIn() }
			f: newFile(
				newFunc("f", withStatements(&ast.ExpressionStatement{
					Expression: &ast.CallExpression{
						Function:  &ast.Identifier{Name: "myCustomBuiltIn"},
						Arguments: nil,
					},
				})),
			),
			builtins: []string{"myCustomBuiltIn"},
		},
		{
			// var x int = f(x);
			// fn f() int {}
			f: newFile(
				newVar("x", newBasicT(token.Int), varWithExpr(&ast.CallExpression{
					Function:  &ast.Identifier{Name: "f"},
					Arguments: []ast.Expression{&ast.Identifier{Name: "x"}},
				})),
				newFunc("f", withResult(newBasicT(token.Int))),
			),
			expect: nil,
		},
	}

	for id, tc := range cases {
		tc := tc
		t.Run(fmt.Sprintf("case-%d", id), func(t *testing.T) {

			_, err := Analyze(tc.f, WithBuiltin(tc.builtins...))
			if !errors.Is(err, tc.expect) {
				t.Errorf("expect error '%v', got '%v'", tc.expect, err)
			}
		})
	}
}
