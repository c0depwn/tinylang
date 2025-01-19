package types

import (
	"errors"
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"strings"
)

type typeError struct {
	msg   string
	parts []string
}

func newTypeError(msg string) typeError {
	return typeError{
		msg: fmt.Sprintf("type error: %s", msg),
	}
}

func (e typeError) WithExpect(expected ast.Type) typeError {
	return typeError{
		msg:   e.msg,
		parts: append(e.parts, fmt.Sprintf("expected '%s'", expected)),
	}
}

func (e typeError) WithExpectOneOf(expected ...ast.Type) typeError {
	return typeError{
		msg:   e.msg,
		parts: append(e.parts, fmt.Sprintf("expected one of '%+v'", expected)),
	}
}

func (e typeError) WithActual(actual ast.Type) typeError {
	return typeError{
		msg:   e.msg,
		parts: append(e.parts, fmt.Sprintf("got '%s'", actual)),
	}
}

func (e typeError) Is(err error) bool {
	var other typeError
	ok := errors.As(err, &other)
	if !ok {
		return false
	}

	if other.msg != e.msg {
		return false
	}
	if len(other.parts) != len(e.parts) {
		return false
	}

	for idx := range other.parts {
		if other.parts[idx] != e.parts[idx] {
			return false
		}
	}

	return true
}

func (e typeError) Error() string {
	if len(e.parts) > 0 {
		return fmt.Sprintf("%s: %s", e.msg, strings.Join(e.parts, ", "))
	}
	return e.msg
}
