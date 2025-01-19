package symbols

import (
	"errors"
	"fmt"
)

type symbolError struct {
	msg string
}

func newSymbolError(msg string) symbolError {
	return symbolError{msg}
}

func newSymbolErrorF(format string, args ...any) symbolError {
	return newSymbolError(fmt.Sprintf(format, args...))
}

func (se symbolError) Error() string {
	return fmt.Sprintf("symbol error: %s", se.msg)
}

func (se symbolError) Is(target error) bool {
	var other symbolError
	if !errors.As(target, &other) {
		return false
	}
	return other.msg == se.msg
}
