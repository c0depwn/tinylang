package initialization

import (
	"errors"
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/symbols"
	"slices"
	"strings"
)

type Initable interface {
	Name() string
	SetInitOrder(int)
	GetInitOrder() int
}

type graph struct {
	adjList map[Initable][]Initable
}

// addNode adds a new node to the graph.
// addNode is idempotent, adding the same node
// more than once will not cause there to be duplicates.
func (g *graph) addNode(n Initable) {
	_, exists := g.adjList[n]
	if exists {
		return
	}

	// alloc adjacency list for node
	g.adjList[n] = []Initable{}
}

// addEdge adds a directed edge from->to.
func (g *graph) addEdge(from, to Initable) {
	_, ok := g.adjList[from]

	// initialize new underlying array if from is an
	// unknown node
	if !ok {
		g.adjList[from] = []Initable{to}
		return
	}

	// avoid duplicate edges
	if slices.Contains(g.adjList[from], to) {
		return
	}

	g.adjList[from] = append(g.adjList[from], to)
}

// ordered returns a slice containing nodes topologically sorted
// using Kahn's Algorithm. The bool return value indicates
// whether cycles were detected, which also indicates that
// the sorting result is unusable.
// The result is sorted descending meaning,
// the nodes with the most dependencies are at the lower
// indices.
func (g *graph) ordered() ([]Initable, bool) {
	if g == nil {
		panic(errors.New("logic error: graph cannot be nil"))
	}
	if len(g.adjList) == 0 {
		return []Initable{}, true
	}

	// calculate in degree for each node
	indegrees := make(map[Initable]int)
	for n := range g.adjList {
		indegrees[n] = 0
	}
	for _, neighbours := range g.adjList {
		for _, neighbour := range neighbours {
			indegrees[neighbour] += 1
		}
	}

	// add all nodes with indeg 0 to queue
	queue := make([]Initable, 0)
	for node, indeg := range indegrees {
		if indeg == 0 {
			queue = append(queue, node)
		}
	}

	ordered := make([]Initable, 0)
	for len(queue) > 0 {
		// dequeue
		node := queue[0]
		queue = queue[1:]

		// save to topologically ordered result
		ordered = append(ordered, node)

		// decrement in-degree for each neighbour
		// adding it to the queue if it reaches 0
		for _, neighbour := range g.adjList[node] {
			indegrees[neighbour] -= 1

			if indegrees[neighbour] == 0 {
				queue = append(queue, neighbour)
			}
		}
	}

	// The sort is only valid if the number of nodes
	// in the ordered slice matches the number of
	// nodes the graph contains. Additionally,
	// this indicates that the graph has no
	// cycles and is therefore a DAG.
	valid := len(ordered) == len(g.adjList)

	if !valid {
		missing := make([]string, 0)
		for k, _ := range g.adjList {
			if !slices.Contains(ordered, k) {
				missing = append(missing, k.Name())
			}
		}
		fmt.Println("the following are missing from the nodes:\n" + strings.Join(missing, "\n"))
	}

	return ordered, valid
}

func (g *graph) String() string {
	var str []string
	for k, v := range g.adjList {
		edgeNames := make([]string, len(v))
		for i, to := range v {
			edgeNames[i] = to.Name()
		}

		str = append(str, fmt.Sprintf(
			"%s -> [%s]",
			k.Name(),
			strings.Join(edgeNames, ","),
		))
	}

	return strings.Join(str, "\n")
}

// TODO: improvement: this func is too big and probably does some unnecessary stuff
//
//	splitting things up will increase readability and testability.
func order(declarations []ast.Declaration, info symbols.Info) error {
	g := &graph{adjList: make(map[Initable][]Initable)}

	for _, d := range declarations {
		if varDecl, ok := d.(*ast.VarDeclaration); ok {
			// add the variable as a node
			g.addNode(varDecl)

			// not all variables have an initialization expression
			if varDecl.Expression == nil {
				continue
			}

			// traverse variable expression
			traversalStack := []ast.Node{}
			ast.Inspect(varDecl.Expression, func(n ast.Node) bool {
				if n == nil {
					// pop
					popped := traversalStack[len(traversalStack)-1]
					traversalStack = traversalStack[:len(traversalStack)-1]
					// adjust scope info
					info.After(popped)
					return true
				}

				// push
				traversalStack = append(traversalStack, n)
				// adjust scope info
				info.Before(n)

				if ident, isIdent := n.(*ast.Identifier); isIdent {
					decl := info.FindInScope(ident.Name)
					if decl == nil {
						panic("declaration not found: " + ident.Name)
					}
					switch specificDecl := decl.(type) {
					case *ast.FuncDeclaration:
						// funcs are not needed in dependency graph
						// and do not need to be traversed any further
						return false
					case *ast.VarDeclaration:
						g.addEdge(varDecl, specificDecl)
					case *ast.ConstDeclaration:
						g.addEdge(varDecl, specificDecl)
					default:
						panic(fmt.Sprintf("unexpected declaration type: %T", decl))
					}
				}

				// traverse but don't register non-identifier nodes into the graph
				return true
			})
		}
	}

	ordered, valid := g.ordered()
	if !valid {
		return fmt.Errorf("cyclic variable initialization detected")
	}

	for i := 0; i < len(ordered); i++ {
		ordered[i].SetInitOrder(len(ordered) - i - 1)
	}

	return nil
}
