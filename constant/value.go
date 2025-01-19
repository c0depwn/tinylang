package constant

import (
	"fmt"
	"github.com/c0depwn/tinylang/token"
	"strconv"
)

type Type int

const (
	Illegal = Type(iota)
	Bool
	String
	Int
)

func (t Type) String() string {
	switch t {
	case Illegal:
		return "illegal"
	case Int:
		return "int"
	case String:
		return "string"
	case Bool:
		return "bool"
	default:
		panic("unknown type")
	}
}

type Value interface {
	// Type returns the Type of the Value.
	Type() Type
	// String returns the original literal of the Value.
	String() string
	// any must return the underlying value.
	any() any
}

func FromLiteral(literal token.Token) Value {
	switch literal.Type {
	case token.IntegerLit:
		return makeInt(literal)
	case token.StringLit:
		return makeString(literal)
	case token.True:
		return makeBool(literal)
	case token.False:
		return makeBool(literal)
	default:
		panic(fmt.Errorf("unexpected literal token type %s", literal.Type))
	}
}

func AsInt32(value Value) int32 {
	// TODO: safe conversion and checks
	return int32(value.(intValue).v)
}

func AsInt(value Value) int {
	// TODO: safe conversion and checks
	return int(value.(intValue).v)
}

func AsBool(value Value) bool {
	return value.(boolValue).v
}

func As[T any](value Value) (T, bool) {
	v, ok := (value.any()).(T)
	return v, ok
}

type boolValue struct {
	literal string
	v       bool
}

func (b boolValue) Type() Type {
	return Bool
}

func (b boolValue) String() string {
	return b.literal
}

func (b boolValue) any() any {
	return b.v
}

func makeBool(t token.Token) Value {
	// precondition
	if t.Type != token.True && t.Type != token.False {
		panic(fmt.Errorf("unexpected literal token type %s", t.Type))
	}

	v, err := strconv.ParseBool(t.Literal)
	if err != nil {
		panic(fmt.Errorf("failed to convert literal to bool: %w", err))
	}

	return boolValue{
		literal: t.Literal,
		v:       v,
	}
}

type stringValue struct {
	literal string
	v       string
}

func (str stringValue) Type() Type {
	return String
}

func (str stringValue) String() string {
	return str.literal
}

func (str stringValue) any() any {
	return str.v
}

func makeString(t token.Token) Value {
	return stringValue{
		literal: t.Literal,
		v:       t.Literal,
	}
}

type intValue struct {
	literal string
	v       int64
}

func (iv intValue) Type() Type {
	return Int
}

func (iv intValue) String() string {
	return iv.literal
}

func (iv intValue) any() any {
	return iv.v
}

func makeInt(t token.Token) Value {
	// precondition
	if t.Type != token.IntegerLit {
		panic(fmt.Errorf("unexpected literal token type %s", t.Type))
	}

	// by default, int is a signed 32-bit integer
	intVal, err := strconv.ParseInt(t.Literal, 0, 64)
	if err != nil {
		panic(fmt.Errorf("failed to convert literal to int: %w", err))
	}

	return intValue{
		literal: t.Literal,
		v:       intVal,
	}
}
