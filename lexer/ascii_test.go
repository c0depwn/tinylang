package lexer

import (
	"bufio"
	"strconv"
	"strings"
	"testing"
)

// TestLexer_acceptASCII ensures all ascii characters are accepted by the Lexer.
func TestLexer_acceptASCII(t *testing.T) {
	for i := byte(0); i < 1<<7; i++ {
		t.Run(strconv.Itoa(int(i)), func(t *testing.T) {
			t.Parallel()

			buf := string([]byte{i})
			reader := bufio.NewReader(strings.NewReader(buf))
			if _, err := readASCIIChar(reader); err != nil {
				t.Fatalf("expected no error got: %v", err)
			}
		})
	}
}

// TestLexer_rejectUnicode ensures that no unicode characters are accepted.
func TestLexer_rejectUnicode(t *testing.T) {
	tests := []struct {
		input string
	}{
		{"😀"},
		{"⌘"},
	}
	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			r := bufio.NewReader(strings.NewReader(test.input))
			_, err := readASCIIChar(r)
			if err == nil {
				t.Fatal("expected error but got none")
			}
		})
	}

}
