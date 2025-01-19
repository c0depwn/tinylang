package parser

import "github.com/c0depwn/tinylang/token"

// TokenSource defines the source for parser tokens.
// The parser will consume tokens until encountering token.EOF.
type TokenSource interface {
	// Next must provide the next token from the underlying source.
	// When no more tokens are available [token.EOF] must be returned.
	// If malformed tokens are encountered, [token.Illegal] must be returned.
	Next() token.Token
}

// This enforces commentIgnorer to implement TokenSource explicitly
// and enables LSPs to report errors prior to compilation.
var _ TokenSource = (*commentIgnorer)(nil)

// commentIgnorer wraps the original TokenSource and
// ignores all tokens of type token.Comment.
type commentIgnorer struct {
	src TokenSource
}

func ignoreCommentsFrom(source TokenSource) TokenSource {
	return &commentIgnorer{source}
}

func (c commentIgnorer) Next() token.Token {
	t := c.src.Next()
	for t.Type == token.Comment {
		t = c.src.Next()
	}
	return t
}
