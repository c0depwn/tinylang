package ext

// CatchPanic implements a wrapper for panic recovery using
// the standard, built-in recover mechanism.
// The cause of the panic will be returned as an error
// if the panic was an error otherwise,
// the panic is propagated.
func CatchPanic(f func()) (err error) {
	defer func() {
		if p := recover(); p != nil {
			if e, ok := p.(error); ok {
				err = e
			} else {
				panic(p)
			}
		}
	}()

	f()
	return nil
}
