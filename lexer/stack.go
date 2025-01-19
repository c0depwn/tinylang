package lexer

// stack stores characters which are being read during a Next call in the Lexer.
// The stack is used to improve memory usage, by avoiding re-allocating a new slice on each Next call.
// The slice (contents) and thereby the underlying array, is re-used on each Next call
// and extended only when the capacity is insufficient.
type stack struct {
	// contents is the underlying slice where elements of the stack are stored.
	contents []byte
	// top points to the next free index in contents.
	top int
}

func newStack() *stack {
	return &stack{
		contents: make([]byte, 16),
		top:      0,
	}
}

// push appends b to the underlying stack.
func (s *stack) push(b byte) {
	if s.top < len(s.contents) {
		s.contents[s.top] = b
	} else {
		s.contents = append(s.contents, b)
	}

	s.top += 1
}

// clear resets the stack top pointer.
func (s *stack) clear() {
	s.top = 0
}

func (s *stack) String() string {
	return string(s.contents[:s.top])
}
