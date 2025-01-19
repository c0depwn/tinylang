package ast

import (
	"fmt"
)

// Type represents a type within TinyLang.
// All types must implement the Type interface.
type Type interface {
	// String provides a string representation of a type.
	String() string

	// Equals must return true if the supplied Type
	// matches the callee. Additionally, equality
	// requires having the same Underlying Type.
	// Other constraints can be imposed by the
	// specific Type.
	Equals(Type) bool

	// Underlying returns the wrapped Type.
	// E.g. "*int" would return "int" as its underlying type.
	// If there is no underlying Type the Type itself
	// must be returned.
	Underlying() Type
}

// TypeIdentifier = ArrayType | BasicTypeName
type TypeIdentifier interface {
	Node
	aTypeIdentifier()
}

type typeIdentifier struct {
	node
}

func (typeIdentifier) aTypeIdentifier() {}

// BasicTypeName = "int" | "bool" | "string"
type BasicTypeName struct {
	typeIdentifier
	Name string
}

func (tn BasicTypeName) String() string {
	return tn.Name
}

// ArrayType = "[" Len "]" ElementType
type ArrayType struct {
	typeIdentifier
	Len         *BasicLiteral
	ElementType TypeIdentifier
}

func (t *ArrayType) String() string {
	return fmt.Sprintf("[%s]%s", t.Len.String(), t.ElementType)
}
