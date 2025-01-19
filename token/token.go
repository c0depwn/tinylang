package token

import (
	"fmt"
)

const (
	// CONTROL

	Illegal = "Illegal"
	EOF     = "EOF"

	// Identifiers & Literals

	Identifier = "Identifier"

	IntegerLit = "Integer"
	StringLit  = "String"

	// Comments

	Comment = "Line Comment"

	// Assignments & Operators

	Assign       = "="
	AssignMul    = "*="
	AssignDiv    = "/="
	AssignMod    = "%="
	AssignAdd    = "+="
	AssignSub    = "-="
	AssignShiftL = "<<="
	AssignShiftR = ">>="
	AssignBitAnd = "&="
	AssignBitOr  = "|="
	AssignBitXor = "^="

	Mul        = "*"
	Div        = "/"
	Mod        = "%"
	Sum        = "+"
	Sub        = "-"
	BitShiftL  = "<<"
	BitShiftR  = ">>"
	BitwiseOr  = "|"
	BitwiseAnd = "&"
	BitwiseXor = "^"
	LogicalOr  = "||"
	LogicalAnd = "&&"

	Excl = "!"

	LessThan         = "<"
	GreaterThan      = ">"
	Equal            = "=="
	NotEqual         = "!="
	LessThanEqual    = "<="
	GreaterThanEqual = ">="

	// Delimiters

	Comma     = ","
	Semicolon = ";"
	LParen    = "("
	RParen    = ")"
	LBrace    = "{"
	RBrace    = "}"
	LBracket  = "["
	RBracket  = "]"

	// Keywords

	Function = "fn"
	Var      = "var"
	Const    = "const"
	True     = "true"
	False    = "false"
	If       = "if"
	Else     = "else"
	While    = "while"
	For      = "for"
	Return   = "return"

	// reserved type names

	Int    = "int"
	Bool   = "bool"
	String = "string"
)

var keywords = map[string]Type{
	"var":    Var,
	"const":  Const,
	"true":   True,
	"false":  False,
	"if":     If,
	"else":   Else,
	"while":  While,
	"for":    For,
	"fn":     Function,
	"return": Return,
	"int":    Int,
	"bool":   Bool,
	"string": String,
}

type Type string

// Token defines a valid language token.
// Literal contains the tokens unchanged literal value as specified in the source code.
type Token struct {
	Type     Type
	Literal  string
	Position Position
}

type Position struct {
	Row, Col int
}

func (t Token) String() string {
	return fmt.Sprintf(
		"TypeID='%s', Literal='%s', Row='%d' Col='%d'",
		t.Type, t.Literal, t.Position.Row, t.Position.Col,
	)
}

func IsReservedKeyword(identifier string) bool {
	_, ok := keywords[identifier]
	return ok
}

// LookupIdentifier checks if the supplied identifier is a reserved keyword.
func LookupIdentifier(ident string) Type {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return Identifier
}

var (
	AssignmentTypes = []Type{
		Assign,
		AssignMul,
		AssignDiv,
		AssignMod,
		AssignAdd,
		AssignSub,
		AssignBitAnd,
		AssignBitOr,
		AssignBitXor,
		AssignShiftL,
		AssignShiftR,
	}

	CompoundAssignmentTypes = []Type{
		AssignMul,
		AssignDiv,
		AssignMod,
		AssignAdd,
		AssignSub,
		AssignBitAnd,
		AssignBitOr,
		AssignBitXor,
		AssignShiftL,
		AssignShiftR,
	}
	CompoundAssignmentOp = map[Type]Type{
		AssignMul:    Mul,
		AssignDiv:    Div,
		AssignMod:    Mod,
		AssignAdd:    Sum,
		AssignSub:    Sub,
		AssignBitAnd: BitwiseAnd,
		AssignBitOr:  BitwiseOr,
		AssignBitXor: BitwiseXor,
		AssignShiftL: BitShiftL,
		AssignShiftR: BitShiftR,
	}
)
