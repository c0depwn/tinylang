package ext

// Stack is a generic implementation of a Stack.
type Stack[T any] []T

func (s *Stack[T]) Push(v T) {
	*s = append(*s, v)
}

func (s *Stack[T]) Pop() T {
	top := (*s)[len(*s)-1]
	*s = (*s)[:len(*s)-1]
	return top
}

func (s *Stack[T]) Top() T {
	return (*s)[len(*s)-1]
}
