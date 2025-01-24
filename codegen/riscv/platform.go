package riscv

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/types"
)

type PlatformID string

const (
	PlatformRV32 PlatformID = "rv32"
	PlatformRV64 PlatformID = "rv64"
)

type Platform interface {
	RegisterSize() int
	SizeOf(ast.Type) int
}

type RV32Platform struct{}

func (p RV32Platform) RegisterSize() int {
	return 4
}

// SizeOf reports the size of the supplied [ast.Type].
// Currently, everything is 4 bytes.
func (p RV32Platform) SizeOf(t ast.Type) int {
	switch t.(type) {
	case types.Byte:
		return 1
	case types.Int:
	case types.Bool:
	case types.Array:
	case types.String:
	default:
		panic(fmt.Sprintf("type '%T' is incompatible with sizeof", t))
	}
	return p.RegisterSize()
}

type RV64Platform struct{}

func (p RV64Platform) RegisterSize() int {
	return 8
}

// SizeOf reports the size of the supplied [ast.Type].
// Currently, everything is 4 bytes.
func (p RV64Platform) SizeOf(t ast.Type) int {
	switch t.(type) {
	case types.Byte:
		return 1
	case types.Int:
	case types.Bool:
	case types.Array:
	case types.String:
	default:
		panic(fmt.Sprintf("type '%T' is incompatible with sizeof", t))
	}
	return p.RegisterSize()
}
