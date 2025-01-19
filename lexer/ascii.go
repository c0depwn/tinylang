package lexer

import (
	"bufio"
	"fmt"
	"io"
	"unicode"
)

func isWhitespace(char byte) bool {
	return char == ' ' || char == '\t' || char == '\n'
}

func readASCIIChar(src *bufio.Reader) (byte, error) {
	utf8Char, _, err := src.ReadRune()
	if err != nil && err != io.EOF {
		return 0, fmt.Errorf("could not read character from source: %v", err)
	}
	if err == io.EOF {
		return 0, nil
	}

	// currently only UTF-8 encoded ASCII characters are supported
	if utf8Char > unicode.MaxASCII {
		return 0, fmt.Errorf("invalid input: '%c' is not a valid ASCII character", utf8Char)
	}

	return byte(utf8Char), nil
}
