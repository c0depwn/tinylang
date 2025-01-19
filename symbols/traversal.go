package symbols

import "github.com/c0depwn/tinylang/ast"

// Inspect implements a traversal mechanism whilst
// providing pre- and post-traversal hooks.
// The hooks are specifically designed to supply
// relevant Scope information related to the received node.
func Inspect(
	n ast.Node, info Info,
	pre func(node ast.Node, scope *Scope),
	post func(node ast.Node, scope *Scope),
) {
	var stack = make([]ast.Node, 0)
	ast.Inspect(n, func(n ast.Node) bool {
		if n == nil {
			popped := stack[len(stack)-1]
			stack = stack[:len(stack)-1]

			post(n, info.current)

			info.After(popped)
			return true
		}

		pre(n, info.current)

		stack = append(stack, n)
		info.Before(n)

		return true
	})
}
