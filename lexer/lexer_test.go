package lexer

import (
	"github.com/c0depwn/tinylang/token"
	"strings"
	"testing"
)

func TestLexer_Next(t *testing.T) {
	program := `
const dec = 1;
var bin = 0b10;
var hex = 0xff;
fn myFunc() {
	return;
}
`

	expect := []token.Token{
		{Type: token.Const, Literal: "const"},
		{Type: token.Identifier, Literal: "dec"},
		{Type: token.Assign, Literal: "="},
		{Type: token.IntegerLit, Literal: "1"},
		{Type: token.Semicolon, Literal: ";"},

		{Type: token.Var, Literal: "var"},
		{Type: token.Identifier, Literal: "bin"},
		{Type: token.Assign, Literal: "="},
		{Type: token.IntegerLit, Literal: "0b10"},
		{Type: token.Semicolon, Literal: ";"},

		{Type: token.Var, Literal: "var"},
		{Type: token.Identifier, Literal: "hex"},
		{Type: token.Assign, Literal: "="},
		{Type: token.IntegerLit, Literal: "0xff"},
		{Type: token.Semicolon, Literal: ";"},

		{Type: token.Function, Literal: "fn"},
		{Type: token.Identifier, Literal: "myFunc"},
		{Type: token.LParen, Literal: "("},
		{Type: token.RParen, Literal: ")"},
		{Type: token.LBrace, Literal: "{"},
		{Type: token.Return, Literal: "return"},
		{Type: token.Semicolon, Literal: ";"},
		{Type: token.RBrace, Literal: "}"},

		{Type: token.EOF, Literal: ""},
	}

	r := strings.NewReader(program)
	l := New(r)

	for i, test := range expect {
		next := l.Next()

		if next.Type != test.Type {
			t.Fatalf("#%d bad token type: expected %s, got %s", i, test.Type, next.Type)
		}
		if next.Literal != test.Literal {
			t.Fatalf("#%d bad token literal: expected %s, got %s", i, test.Literal, next.Literal)
		}
	}
}

func TestLexer_Empty(t *testing.T) {
	program := ``

	expect := []token.Token{
		{Type: token.EOF, Literal: ""},
	}

	r := strings.NewReader(program)
	l := New(r)

	for i, test := range expect {
		next := l.Next()

		if next.Type != test.Type {
			t.Fatalf("#%d bad token type: expected %s, got %s", i, test.Type, next.Type)
		}
		if next.Literal != test.Literal {
			t.Fatalf("#%d bad token literal: expected %s, got %s", i, test.Literal, next.Literal)
		}
	}
}

func TestLexer_AssignmentTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "=",
			expect: token.Token{Type: token.Assign, Literal: "="},
		},
		{
			input:  "&=",
			expect: token.Token{Type: token.AssignBitAnd, Literal: "&="},
		},
		{
			input:  "|=",
			expect: token.Token{Type: token.AssignBitOr, Literal: "|="},
		},
		{
			input:  "^=",
			expect: token.Token{Type: token.AssignBitXor, Literal: "^="},
		},
		{
			input:  "<<=",
			expect: token.Token{Type: token.AssignShiftL, Literal: "<<="},
		},
		{
			input:  ">>=",
			expect: token.Token{Type: token.AssignShiftR, Literal: ">>="},
		},
		{
			input:  "+=",
			expect: token.Token{Type: token.AssignAdd, Literal: "+="},
		},
		{
			input:  "-=",
			expect: token.Token{Type: token.AssignSub, Literal: "-="},
		},
		{
			input:  "*=",
			expect: token.Token{Type: token.AssignMul, Literal: "*="},
		},
		{
			input:  "/=",
			expect: token.Token{Type: token.AssignDiv, Literal: "/="},
		},
		{
			input:  "%=",
			expect: token.Token{Type: token.AssignMod, Literal: "%="},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_BitWiseTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "|",
			expect: token.Token{Type: token.BitwiseOr, Literal: "|"},
		},
		{
			input:  "&",
			expect: token.Token{Type: token.BitwiseAnd, Literal: "&"},
		},
		{
			input:  "^",
			expect: token.Token{Type: token.BitwiseXor, Literal: "^"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_BitShiftTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "<<",
			expect: token.Token{Type: token.BitShiftL, Literal: "<<"},
		},
		{
			input:  ">>",
			expect: token.Token{Type: token.BitShiftR, Literal: ">>"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_LogicalTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "||",
			expect: token.Token{Type: token.LogicalOr, Literal: "||"},
		},
		{
			input:  "&&",
			expect: token.Token{Type: token.LogicalAnd, Literal: "&&"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_ArithmeticTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "+",
			expect: token.Token{Type: token.Sum, Literal: "+"},
		},
		{
			input:  "-",
			expect: token.Token{Type: token.Sub, Literal: "-"},
		},
		{
			input:  "*",
			expect: token.Token{Type: token.Mul, Literal: "*"},
		},
		{
			input:  "/",
			expect: token.Token{Type: token.Div, Literal: "/"},
		},
		{
			input:  "%",
			expect: token.Token{Type: token.Mod, Literal: "%"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_UnaryOperatorTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "!",
			expect: token.Token{Type: token.Excl, Literal: "!"},
		},
		{
			input:  "-",
			expect: token.Token{Type: token.Sub, Literal: "-"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_EqualityTokens(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "<",
			expect: token.Token{Type: token.LessThan, Literal: "<"},
		},
		{
			input:  ">",
			expect: token.Token{Type: token.GreaterThan, Literal: ">"},
		},
		{
			input:  "==",
			expect: token.Token{Type: token.Equal, Literal: "=="},
		},
		{
			input:  "!=",
			expect: token.Token{Type: token.NotEqual, Literal: "!="},
		},
		{
			input:  "<=",
			expect: token.Token{Type: token.LessThanEqual, Literal: "<="},
		},
		{
			input:  ">=",
			expect: token.Token{Type: token.GreaterThanEqual, Literal: ">="},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_Delimiters(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  ",",
			expect: token.Token{Type: token.Comma, Literal: ","},
		},
		{
			input:  ";",
			expect: token.Token{Type: token.Semicolon, Literal: ";"},
		},
		{
			input:  "(",
			expect: token.Token{Type: token.LParen, Literal: "("},
		},
		{
			input:  ")",
			expect: token.Token{Type: token.RParen, Literal: ")"},
		},
		{
			input:  "{",
			expect: token.Token{Type: token.LBrace, Literal: "{"},
		},
		{
			input:  "}",
			expect: token.Token{Type: token.RBrace, Literal: "}"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_Keywords(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "var",
			expect: token.Token{Type: token.Var, Literal: "var"},
		},
		{
			input:  "const",
			expect: token.Token{Type: token.Const, Literal: "const"},
		},
		{
			input:  "true",
			expect: token.Token{Type: token.True, Literal: "true"},
		},
		{
			input:  "false",
			expect: token.Token{Type: token.False, Literal: "false"},
		},
		{
			input:  "if",
			expect: token.Token{Type: token.If, Literal: "if"},
		},
		{
			input:  "else",
			expect: token.Token{Type: token.Else, Literal: "else"},
		},
		{
			input:  "for",
			expect: token.Token{Type: token.For, Literal: "for"},
		},
		{
			input:  "while",
			expect: token.Token{Type: token.While, Literal: "while"},
		},
		{
			input:  "fn",
			expect: token.Token{Type: token.Function, Literal: "fn"},
		},
		{
			input:  "return",
			expect: token.Token{Type: token.Return, Literal: "return"},
		},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			t.Parallel()
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_IntLiteral(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "0",
			expect: token.Token{Type: token.IntegerLit, Literal: "0"},
		},
		{
			input:  "0b1010",
			expect: token.Token{Type: token.IntegerLit, Literal: "0b1010"},
		},
		{
			input:  "0xABCDEF1234567890",
			expect: token.Token{Type: token.IntegerLit, Literal: "0xABCDEF1234567890"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_StringLiteral(t *testing.T) {
	cases := []struct {
		input  string
		expect token.Token
	}{
		{
			input:  "`asdf`",
			expect: token.Token{Type: token.StringLit, Literal: "asdf"},
		},
		// TODO: eventually, supporting non-ascii characters would be nice,
		// 	this currently fails if left uncommented since multi-byte chars are not allowed
		//{
		//	input:  "\`asdf😇\`",
		//	expect: token.Token{TypeID: token.StringLit, Literal: "asdf"},
		//},
	}

	for _, tc := range cases {
		t.Run(tc.input, func(t *testing.T) {
			l := New(strings.NewReader(tc.input))
			expectSingleToken(t, l, tc.expect)
		})
	}
}

func TestLexer_Comments(t *testing.T) {
	source := `
// before func
fn main() {
    // before variable
    var x int;
    // after variable
}
// after func
`
	expect := []token.Token{
		{Type: token.Comment, Literal: "// before func"},
		{Type: token.Function, Literal: "fn"},
		{Type: token.Identifier, Literal: "main"},
		{Type: token.LParen, Literal: "("},
		{Type: token.RParen, Literal: ")"},
		{Type: token.LBrace, Literal: "{"},
		{Type: token.Comment, Literal: "// before variable"},
		{Type: token.Var, Literal: "var"},
		{Type: token.Identifier, Literal: "x"},
		{Type: token.Int, Literal: "int"},
		{Type: token.Semicolon, Literal: ";"},
		{Type: token.Comment, Literal: "// after variable"},
		{Type: token.RBrace, Literal: "}"},
		{Type: token.Comment, Literal: "// after func"},
	}

	r := strings.NewReader(source)
	l := New(r)

	for i, test := range expect {
		next := l.Next()

		if next.Type != test.Type {
			t.Fatalf("#%d bad token type: expected %s, got %s", i, test.Type, next.Type)
		}
		if next.Literal != test.Literal {
			t.Fatalf("#%d bad token literal: expected %s, got %s", i, test.Literal, next.Literal)
		}
	}
}

func expectSingleToken(t *testing.T, l *Lexer, expect token.Token) {
	actualToken := l.Next()

	if actualToken.Type != expect.Type {
		t.Fatalf("expected '%+v', got '%+v", expect, actualToken)
	}

	if actualToken.Literal != expect.Literal {
		t.Fatalf("expected '%+v', got '%+v", expect, actualToken)
	}

	next := l.Next()
	if next.Type != token.EOF {
		t.Fatalf("expected EOF, got '%v'", next)
	}
}
