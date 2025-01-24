package constant

import (
	"errors"
	"fmt"
	"github.com/c0depwn/tinylang/token"
	"math/big"
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
	var (
		v Value
		e error
	)

	switch literal.Type {
	case token.IntegerLit:
		v, e = makeInt(literal)
	case token.StringLit:
		v = makeString(literal)
	case token.True:
		v, e = makeBool(literal)
	case token.False:
		v, e = makeBool(literal)
	default:
		panic(fmt.Errorf("unexpected literal token type %s", literal.Type))
	}

	if e != nil {
		panic(e)
	}

	return v
}

func AsInt(value Value) (int, error) {
	intVal, ok := value.(intValue)
	if !ok {
		return 0, errors.New("value is not an integer")
	}

	if intVal.v.BitLen() > 64 {
		return 0, errors.New("value is too big for signed int")
	}

	return int(intVal.v.Int64()), nil
}

func AsByte(value Value) (byte, error) {
	// all numeric literals are currently int literals
	v, ok := value.(intValue)
	if !ok {
		return 0, errors.New("value is not an integer")
	}
	if v.v.BitLen() > 8 {
		return 0, fmt.Errorf("value is too big for byte need at least %d bits", v.v.BitLen())
	}
	return byte(v.v.Uint64()), nil
}

func AsBool(value Value) (bool, error) {
	boolValue, ok := value.(boolValue)
	if !ok {
		return false, errors.New("value is not a boolean")
	}
	return boolValue.v, nil
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

func makeBool(t token.Token) (Value, error) {
	// precondition
	if t.Type != token.True && t.Type != token.False {
		panic(fmt.Errorf("unexpected literal token type %s", t.Type))
	}

	v, err := strconv.ParseBool(t.Literal)
	if err != nil {
		return nil, fmt.Errorf("failed to convert literal to bool: %w", err)
	}

	return boolValue{
		literal: t.Literal,
		v:       v,
	}, nil
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
	v       *big.Int
}

func (iv intValue) Type() Type {
	return Int
}

func (iv intValue) String() string {
	return iv.literal
}

func (iv intValue) any() any {
	return iv.v.Int64()
}

func makeInt(t token.Token) (Value, error) {
	// precondition
	if t.Type != token.IntegerLit {
		panic(fmt.Errorf("unexpected literal token type %s", t.Type))
	}

	bigInt := new(big.Int)
	bigInt, ok := bigInt.SetString(t.Literal, 0)
	if !ok {
		return nil, fmt.Errorf("%s is not a valid integer", t.Literal)
	}

	return intValue{
		literal: t.Literal,
		v:       bigInt,
	}, nil
}
