package types

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	ext "github.com/c0depwn/tinylang/pkg/slices"
	"github.com/c0depwn/tinylang/token"
	"reflect"
	"slices"
	"strings"
)

type Int struct {
	c *constant.Value
}

func NewInt() Int {
	return Int{}
}

func (i Int) String() string {
	return "int"
}

func (i Int) Equals(some ast.Type) bool {
	_, ok := some.(Int)
	return ok
}

func (i Int) setConst(c constant.Value) Int {
	i.c = &c
	return i
}

func (i Int) Underlying() ast.Type {
	return i
}

type Byte struct{}

func NewByte() Byte {
	return Byte{}
}

func (i Byte) String() string {
	return "byte"
}

func (i Byte) Equals(some ast.Type) bool {
	_, ok := some.(Byte)
	return ok
}

func (i Byte) Underlying() ast.Type {
	return i
}

type Bool struct{}

func NewBool() Bool {
	return Bool{}
}

func (b Bool) String() string {
	return "bool"
}

func (b Bool) Equals(some ast.Type) bool {
	_, ok := some.(Bool)
	return ok
}

func (b Bool) Underlying() ast.Type {
	return b
}

type String struct {
	Length uint
}

func NewString(len uint) String {
	return String{Length: len}
}

func (s String) String() string {
	return "string"
}

func (s String) Equals(some ast.Type) bool {
	_, ok := some.(String)
	return ok
}

func (s String) Underlying() ast.Type {
	return s
}

type Array struct {
	Element ast.Type
	Length  uint
}

func NewArray(len uint, elem ast.Type) Array {
	return Array{Element: elem, Length: len}
}

func (a Array) String() string {
	return fmt.Sprintf("[%d]%s", a.Length, a.Element.String())
}

func (a Array) Equals(some ast.Type) bool {
	other, ok := some.(Array)
	if !ok {
		return false
	}
	if a.Length != other.Length {
		return false
	}
	return other.Element.Equals(a.Element)
}

func (a Array) Underlying() ast.Type {
	return a
}

func (a Array) Dimensions() int {
	d := 1
	t := a
	for {
		elemT, ok := t.Element.(Array)
		if !ok {
			break
		}
		d++
		t = elemT
	}
	return d
}

// Dimension returns the ast.Type at the specified dimension
// of the Array.
// Dimension 0 represents the Array itself.
// Dimension 1 represents the Element of the Array.
// Dimension 2 represents the Element of the Element
// and so on.
//
// Example:
// Given the Array [x][y]int (2D)
// Dimensions(0) -> [x][y]int
// Dimensions(1) -> [y]int
// Dimensions(2) -> int
// Dimensions(3) -> nil
func (a Array) Dimension(d int) ast.Type {
	if d == 0 {
		return a
	}

	t := a
	for current := 1; current < d; current++ {
		inner, ok := t.Element.(Array)
		if !ok {
			return nil
		}
		t = inner
	}
	return t.Element
}

type Function struct {
	Params []ast.Type
	Result ast.Type
}

func NewFunction(res ast.Type, params []ast.Type) Function {
	return Function{Params: params, Result: res}
}

func (f Function) String() string {
	str := ext.Map(f.Params, func(p ast.Type) string { return p.String() })
	return fmt.Sprintf("fn(%s) %s", strings.Join(str, ","), f.Result.String())
}

func (f Function) Equals(some ast.Type) bool {
	other, ok := some.(Function)
	if !ok {
		return false
	}
	return other.String() == f.String()
}

func (f Function) Underlying() ast.Type {
	return f
}

type Pointer struct {
	to ast.Type
}

func NewPtr(to ast.Type) Pointer {
	return Pointer{to: to}
}

func (t Pointer) String() string {
	return fmt.Sprintf("*%s", t.to.String())
}

func (t Pointer) Equals(some ast.Type) bool {
	other, ok := some.(Pointer)
	if !ok {
		return false
	}
	return other.to.Equals(t.to)
}

func (t Pointer) Underlying() ast.Type {
	return t.to
}

// Void is an [ast.Type] which represents the absence of a type.
type Void struct{}

func NewVoid() Void {
	return Void{}
}

func (v Void) String() string {
	return "void"
}

func (v Void) Equals(some ast.Type) bool {
	_, ok := some.(Void)
	return ok
}

func (v Void) Underlying() ast.Type {
	return v
}

func IsPointer(t ast.Type) bool {
	_, ok := t.(Pointer)
	return ok
}

func As[T ast.Type](t ast.Type) T {
	assertedT, ok := t.(T)
	if !ok {
		panic(fmt.Errorf("expected %T, got %T", new(T), t))
	}
	return assertedT
}

var prefixOperatorsByType = map[reflect.Type][]token.Type{
	reflect.TypeOf(Int{}):  {token.Sub, token.Sum},
	reflect.TypeOf(Byte{}): {token.Sub, token.Sum},
	reflect.TypeOf(Bool{}): {token.Excl},
}

// TODO: improvement: mapping operators to built in functions
//
//	could be interesting to gain operator overloading
//	like functionality e.g. + becomes +(a, b)
//
// this might also ease code generation flexibility
// and allow easily adding type specific implementations
var infixOperatorsByType = map[reflect.Type][]token.Type{
	reflect.TypeOf(Int{}): {
		token.Sum,
		token.Sub,
		token.Mul,
		token.Div,
		token.Mod,
		token.BitwiseAnd,
		token.BitwiseOr,
		token.BitwiseXor,
		token.BitShiftL,
		token.BitShiftR,
		// comparison ops
		token.Equal,
		token.NotEqual,
		token.LessThan,
		token.LessThanEqual,
		token.GreaterThan,
		token.GreaterThanEqual,
	},
	reflect.TypeOf(Byte{}): {
		token.Sum,
		token.Sub,
		token.Mul,
		token.Div,
		token.Mod,
		token.BitwiseAnd,
		token.BitwiseOr,
		token.BitwiseXor,
		token.BitShiftL,
		token.BitShiftR,
		// comparison ops
		token.Equal,
		token.NotEqual,
		token.LessThan,
		token.LessThanEqual,
		token.GreaterThan,
		token.GreaterThanEqual,
	},
	reflect.TypeOf(Bool{}): {
		token.LogicalAnd,
		token.LogicalOr,
	},
	reflect.TypeOf(String{}): {
		token.Sum,
		token.Equal,
	},
}

// hasPrefixOperator reports whether the supplied ast.Type
// is compatible with the operator.
func hasPrefixOperator(operator token.Type, t ast.Type) bool {
	tt := reflect.TypeOf(t)
	operators, ok := prefixOperatorsByType[tt]
	if !ok {
		return false
	}
	return slices.Contains(operators, operator)
}

// hasInfixOperator reports whether the supplied ast.Type
// is compatible with the operator.
// Compatibility means that a computation using the operator
// is possible using two operands of ast.Type.
func hasInfixOperator(operator token.Type, t ast.Type) bool {
	tt := reflect.TypeOf(t)
	operators, ok := infixOperatorsByType[tt]
	if !ok {
		return false
	}
	return slices.Contains(operators, operator)
}

var comparisonOperators = []token.Type{
	token.Equal,
	token.NotEqual,
	token.LessThan,
	token.LessThanEqual,
	token.GreaterThan,
	token.GreaterThanEqual,
}

func isComparison(operator token.Type) bool {
	return slices.Contains(comparisonOperators, operator)
}
