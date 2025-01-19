package lexer

import (
	"bufio"
	"fmt"
	"github.com/c0depwn/tinylang/token"
	"io"
)

// The Lexer reads the source code and produces lexical tokens.
type Lexer struct {
	// char is the current character which is being examined
	char byte
	// src is the source file from which the Lexer is reading the input
	src *bufio.Reader
	// row and column point to the beginning of the last read token.Token
	// in the source file.
	row, column int
	// lastTokenLen contains length of the last token.
	// column is updated using this value on the subsequent Next call.
	lastTokenLen int
	// stack contains the characters consumed from the source
	// which have are being matched to a token.
	stack *stack
}

// New creates and initializes the Lexer.
func New(src io.Reader) *Lexer {
	return &Lexer{
		src:    bufio.NewReader(src),
		char:   0,
		row:    1,
		column: 1,
		stack:  newStack(),
	}
}

// Next reads the underlying source until a token is discovered.
// When the underlying source has been read to completion a
// token.Token of token.Type token.EOF is returned.
// When the Lexer is unable to recognize a token.Token,
// a token.Token of token.Type token.Illegal is returned.
func (l *Lexer) Next() token.Token {
	// skip leading whitespace
	l.skipWhitespace()

	// set the initial state of the state-machine
	state := start
	l.stack.clear()

	for {
		// attempt to transition to the next state using the peeked character
		next := l.peekChar()

		// TODO: remove, this is just for debugging purposes
		//fmt.Printf("'%c' state=%v\n", next, state)

		nextState := transitions[state][next]

		// if the next state is invalid, the current state is a potential
		// accepting state which can be mapped to a token.Token
		if nextState == 0 {
			break
		}

		// consume the peeked character
		l.readChar()

		// push current character onto stack for literal construction
		if !l.shouldExclude(state, l.char) {
			l.stack.push(l.char)
		}

		// transition state
		state = nextState
	}

	literal := l.stack.String()

	t := constructToken(state, literal, l.row, l.column)

	// The position always points to the beginning of a token therefore,
	// the column is updated after constructing the token
	l.column += len(literal)

	return t
}

func constructToken(state lexerState, literal string, r, c int) token.Token {
	t := fromState(state, literal, r, c)

	if t.Type == "" {
		return token.Token{Type: token.Illegal, Literal: literal}
	}

	return t
}

func (l *Lexer) readChar() byte {
	b, err := readASCIIChar(l.src)
	if err != nil {
		panic(err)
	}

	l.char = b

	l.row, l.column = updatePosition(b, l.row, l.column)

	return b
}

func (l *Lexer) peekChar() byte {
	buf, err := l.src.Peek(1)
	if err != nil && err != io.EOF {
		panic(fmt.Errorf("could not peek char: %v", err))
	}
	if err == io.EOF {
		return 0
	}

	return buf[0]
}

func (l *Lexer) skipWhitespace() {
	for p := l.peekChar(); isWhitespace(p); p = l.peekChar() {
		l.readChar()
	}
}

func updatePosition(char byte, row, col int) (int, int) {
	switch char {
	case '\n':
		return row + 1, 1
	case '\t':
		return row, col + 4
	case ' ':
		return row, col + 1
	}
	return row, col
}

// shouldExclude decides whether the character should be added to the literal stack or not.
// This is mainly used to ignore literals which have a specific encoding.
// A specific example is string literals, the backticks are not included in the actual literal of the token.
func (l *Lexer) shouldExclude(state lexerState, char byte) bool {
	return state == start && char == '`' ||
		state == stateBacktick && char == '`' ||
		state == statePartialStringLiteral && char == '`'
}
