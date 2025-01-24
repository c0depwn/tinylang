package parser

import (
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/token"
)

type rightBindingPower = int

const (
	_ rightBindingPower = iota
	Lowest
	Logic    // &&, ||
	Equality // ==, <=, >=, !=, <, >
	Bitwise  // &, |, ^
	BitShift // <<, >>
	Sum      // +, -
	Product  // *, /, %
	Unary    // -, !
	Accessor // [, ., (
)

// precedences contains the right-binding-power (rbp) for each token.Type
// A lower rbp means that the token.Type binds weaker to the right.
// Example: "+" has a lower rbp than "*" therefore A + B * C => A+(B*C).
var precedences = map[token.Type]rightBindingPower{
	token.Mul: Product,
	token.Div: Product,
	token.Mod: Product,

	token.Sum: Sum,
	token.Sub: Sum,

	token.BitShiftR: BitShift,
	token.BitShiftL: BitShift,

	token.BitwiseAnd: Bitwise,
	token.BitwiseOr:  Bitwise,
	token.BitwiseXor: Bitwise,

	token.Equal:            Equality,
	token.NotEqual:         Equality,
	token.LessThan:         Equality,
	token.GreaterThan:      Equality,
	token.LessThanEqual:    Equality,
	token.GreaterThanEqual: Equality,

	token.LogicalAnd: Logic,
	token.LogicalOr:  Logic,

	token.LBracket: Accessor,
	token.LParen:   Accessor,
}

// rbpOf returns the right-binding-power for a specific token.Type according to the precedences table.
func rbpOf(tokenType token.Type) int {
	if rbp, ok := precedences[tokenType]; ok {
		return rbp
	}
	return Lowest
}

// nud (null denotation) parses and returns an ast.Expression representing prefix expression, variables or literals.
// Unliked led, nud does not care about the tokens on the left.
//
// Example:
//
//	input: "!x"
//	resulting ast.Expression:
//			"!"
//	           \
//	            x
type nud = func() ast.Expression

// led (left-denotation) takes the left hand side of an expression and returns
// the ast.Expression representing a fully parsed infix expression.
//
//			  ast.Expression <-- the return value
//				/		\
//	           /         \
//			left      remaining <-- the remaining expression, parsed inside function
//
// Example:
//
//	input = "a + b", left = a
//	resulting ast.Expression:
//				+
//	          /   \
//	         a     b
type led = func(left ast.Expression) ast.Expression

type parseFuncs struct {
	prefixFuncs map[token.Type]nud
	infixFuncs  map[token.Type]led
}

func newParseFuncs(p *parser) parseFuncs {
	pf := parseFuncs{
		infixFuncs:  make(map[token.Type]led),
		prefixFuncs: make(map[token.Type]nud),
	}

	pf.registerPrefix(token.Excl, p.parsePrefixExpression)
	pf.registerPrefix(token.Sub, p.parsePrefixExpression)
	pf.registerPrefix(token.Identifier, p.parseIdentifier)
	pf.registerPrefix(token.IntegerLit, p.parseBasicLiteral)
	pf.registerPrefix(token.StringLit, p.parseBasicLiteral)
	pf.registerPrefix(token.True, p.parseBasicLiteral)
	pf.registerPrefix(token.False, p.parseBasicLiteral)
	pf.registerPrefix(token.LParen, p.parseGroupExpression)
	pf.registerPrefix(token.LBracket, p.parseArrayLiteral)
	pf.registerPrefix(token.Byte, p.parseConversion)
	pf.registerPrefix(token.Int, p.parseConversion)
	pf.registerPrefix(token.String, p.parseConversion)

	pf.registerInfix(token.Mul, p.parseInfixExpression)
	pf.registerInfix(token.Div, p.parseInfixExpression)
	pf.registerInfix(token.Mod, p.parseInfixExpression)
	pf.registerInfix(token.Sum, p.parseInfixExpression)
	pf.registerInfix(token.Sub, p.parseInfixExpression)
	pf.registerInfix(token.BitShiftL, p.parseInfixExpression)
	pf.registerInfix(token.BitShiftR, p.parseInfixExpression)
	pf.registerInfix(token.BitwiseAnd, p.parseInfixExpression)
	pf.registerInfix(token.BitwiseOr, p.parseInfixExpression)
	pf.registerInfix(token.BitwiseXor, p.parseInfixExpression)
	pf.registerInfix(token.Equal, p.parseInfixExpression)
	pf.registerInfix(token.NotEqual, p.parseInfixExpression)
	pf.registerInfix(token.LessThanEqual, p.parseInfixExpression)
	pf.registerInfix(token.LessThan, p.parseInfixExpression)
	pf.registerInfix(token.GreaterThan, p.parseInfixExpression)
	pf.registerInfix(token.GreaterThanEqual, p.parseInfixExpression)
	pf.registerInfix(token.LogicalAnd, p.parseInfixExpression)
	pf.registerInfix(token.LogicalOr, p.parseInfixExpression)
	pf.registerInfix(token.LBracket, p.parseIndexExpression)
	pf.registerInfix(token.LParen, p.parseCallExpression)

	return pf
}

func (pf parseFuncs) registerPrefix(t token.Type, f nud) {
	pf.prefixFuncs[t] = f
}

func (pf parseFuncs) registerInfix(t token.Type, f led) {
	pf.infixFuncs[t] = f
}

func (pf parseFuncs) prefix(t token.Type) nud {
	return pf.prefixFuncs[t]
}

func (pf parseFuncs) infix(i token.Type) led {
	return pf.infixFuncs[i]
}
