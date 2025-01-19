package types

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"testing"
)

func TestArray_Dimensions(t *testing.T) {
	cases := []struct {
		arrType            Array
		expectedDimensions int
	}{
		{
			arrType:            NewArray(1, NewInt()),
			expectedDimensions: 1,
		},
		{
			arrType:            NewArray(1, NewArray(1, NewInt())),
			expectedDimensions: 2,
		},
		{
			arrType:            NewArray(1, NewArray(1, NewArray(1, NewInt()))),
			expectedDimensions: 3,
		},
	}
	for _, tc := range cases {
		tc := tc
		t.Run(tc.arrType.String(), func(t *testing.T) {
			actual := tc.arrType.Dimensions()
			if actual != tc.expectedDimensions {
				t.Fatalf("expected %d, got %d", tc.expectedDimensions, actual)
			}
		})
	}
}

func TestArray_Dimension(t *testing.T) {
	cases := []struct {
		arrType   Array
		dimension int
		expectedT ast.Type
	}{
		{
			arrType:   NewArray(1, NewInt()),
			dimension: 0,
			expectedT: NewArray(1, NewInt()),
		},
		{
			arrType:   NewArray(1, NewInt()),
			dimension: 1,
			expectedT: NewInt(),
		},
		{
			arrType:   NewArray(1, NewInt()),
			dimension: 2,
			expectedT: nil,
		},
		{
			arrType:   NewArray(1, NewArray(1, NewInt())),
			dimension: 1,
			expectedT: NewArray(1, NewInt()),
		},
		{
			arrType:   NewArray(1, NewArray(1, NewInt())),
			dimension: 2,
			expectedT: NewInt(),
		},
	}
	for _, tc := range cases {
		tc := tc
		t.Run(tc.arrType.String(), func(t *testing.T) {
			actualT := tc.arrType.Dimension(tc.dimension)

			if actualT != tc.expectedT {
				t.Fatalf("expected %v, got %v", tc.expectedT, actualT)
			}
		})
	}
}

func TestInt_Equals(t *testing.T) {
	if !NewInt().Equals(NewInt()) {
		t.Fatalf("int equality check failed")
	}
}

func TestBool_Equals(t *testing.T) {
	if !NewBool().Equals(NewBool()) {
		t.Fatalf("bool equality check failed")
	}
}

func TestString_Equals(t *testing.T) {
	cases := []struct {
		a      String
		b      ast.Type
		expect bool
	}{
		{
			a:      NewString(0),
			b:      NewString(0),
			expect: true,
		},
		{
			a:      NewString(0),
			b:      NewString(1),
			expect: true,
		},
		{
			a:      NewString(0),
			b:      NewBool(),
			expect: false,
		},
		{
			a:      NewString(0),
			b:      NewInt(),
			expect: false,
		},
		{
			a:      NewString(0),
			b:      NewArray(0, NewInt()),
			expect: false,
		},
		{
			a:      NewString(0),
			b:      NewPtr(NewString(0)),
			expect: false,
		},
	}

	for i, tc := range cases {
		tc := tc
		t.Run(fmt.Sprintf("case-%d", i), func(t *testing.T) {
			actual := tc.a.Equals(tc.b)
			if actual != tc.expect {
				t.Fatalf("unexpected equality result: expected '%v', got '%v'", tc.expect, actual)
			}
		})
	}
}

func TestFunction_Equals(t *testing.T) {
	cases := []struct {
		a      Function
		b      ast.Type
		expect bool
	}{
		{
			a:      NewFunction(NewVoid(), nil),
			b:      NewFunction(NewVoid(), nil),
			expect: true,
		},
		{
			a:      NewFunction(NewInt(), []ast.Type{NewInt()}),
			b:      NewFunction(NewInt(), []ast.Type{NewInt()}),
			expect: true,
		},
		{
			a:      NewFunction(NewInt(), []ast.Type{NewInt(), NewInt()}),
			b:      NewFunction(NewInt(), []ast.Type{NewInt(), NewInt()}),
			expect: true,
		},
	}

	for i, tc := range cases {
		tc := tc
		t.Run(fmt.Sprintf("case-%d", i), func(t *testing.T) {
			actual := tc.a.Equals(tc.b)
			if actual != tc.expect {
				t.Fatalf("unexpected equality result: expected '%v', got '%v'", tc.expect, actual)
			}
		})
	}
}

func TestPointer_Equals(t *testing.T) {
	if !NewPtr(NewInt()).Equals(NewPtr(NewInt())) {
		t.Fatalf("ptr equality check failed")
	}
}
