package slices

func Filter[S any](s []S, f func(S) bool) []S {
	result := make([]S, 0, len(s))
	for _, item := range s {
		if f(item) {
			result = append(result, item)
		}
	}
	return result
}

func Map[I, O any](s []I, f func(I) O) []O {
	mapped := make([]O, len(s))
	for i := 0; i < len(s); i++ {
		mapped[i] = f(s[i])
	}
	return mapped
}
