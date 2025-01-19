package types

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	"github.com/c0depwn/tinylang/token"
)

// FromBasic creates an [ast.Type] from the supplied [*ast.BasicTypeName].
// This results in a basic [ast.Type] such as bool, int and string.
func FromBasic(identifier *ast.BasicTypeName) ast.Type {
	switch identifier.Name {
	case token.Int:
		return NewInt()
	case token.Bool:
		return NewBool()
	case token.String:
		return NewString(0)
	default:
		panic(fmt.Sprintf("unknown type identifier name: %v", identifier.Name))
	}
}

// FromTypeIdentifier converts an [ast.TypeIdentifier] to a specific [ast.Type].
// This can result in any specific [ast.Type].
func FromTypeIdentifier(id ast.TypeIdentifier) ast.Type {
	if id == nil {
		return NewVoid()
	}

	switch t := id.(type) {
	case *ast.BasicTypeName:
		return FromBasic(t)
	case *ast.ArrayType:
		length := uint32(constant.AsInt(t.Len.Value()))
		return NewArray(length, FromTypeIdentifier(t.ElementType))
	default:
		panic(fmt.Sprintf("unknown type identifier: %+v", id))
	}
}

func FromConstant(c constant.Value) ast.Type {
	switch c.Type() {
	case constant.Int:
		return NewInt()
	case constant.Bool:
		return NewBool()
	case constant.String:
		str, _ := constant.As[string](c)   // TODO: utf-8?
		return NewString(uint32(len(str))) // TODO: size check
	default:
		panic(fmt.Sprintf("unknown constant value type: %+v", c))
	}
}
