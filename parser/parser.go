package parser

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/constant"
	"github.com/c0depwn/tinylang/token"
	"slices"
)

// The parser produces an AST from the received tokens.
// It makes use of a top-down approach with a lookahead of 1 token.
// Tokens are consumed from the configured TokenSource.
// Any encountered errors are delegated to the configured ErrorHandler.
type parser struct {
	// tokens is the source which provides the next token to be parsed
	tokens TokenSource

	// errHandler decides what to do when an error is encountered during parsing
	errHandler ErrorHandler

	// current is the current token.Token which is being parsed
	current token.Token
	// next contains the next available token from tokens
	next token.Token

	// tracer is used to easily trace the parsing path
	tracer tracerI

	// When debug is enabled additional assertions are made.
	// Notice, this can cause the parser to terminate prematurely.
	debug bool

	// parseFuncs contains token.Type specific parse functions for infix and prefix expressions
	parseFuncs
}

func (p *parser) parse() *ast.File {
	root := new(ast.File)
	root.SetPosition(p.position())

	for !p.currentIs(token.EOF) {
		switch p.current.Type {
		case token.Var:
			root.Declarations = append(root.Declarations, p.parseVarDeclaration())
			// ensure the declaration is terminated with a semicolon
			if !p.nextIs(token.Semicolon) {
				p.syntaxError(fmt.Sprintf("expected semicolon after var declaration, got %s", p.current.Type))
			}
			// consume the semicolon
			p.advance()
		case token.Const:
			root.Declarations = append(root.Declarations, p.parseConstDeclaration())
			// ensure the declaration is terminated with a semicolon
			if !p.nextIs(token.Semicolon) {
				p.syntaxError(fmt.Sprintf("expected semicolon after const declaration, got %s", p.current.Type))
			}
			// consume the semicolon
			p.advance()
		case token.Function:
			root.Declarations = append(root.Declarations, p.parseFunctionDeclaration())
		default:
			p.errHandler(fmt.Errorf("unexpected token type %s", p.current.Type))
		}

		// consume the last token of the declaration
		p.advance()
	}

	return root
}

// Block         	= "{" StatementList "}" .
// StatementList   	= { Statement ";" } .
func (p *parser) parseBlock() *ast.Block {
	p.tracer.begin("parseBlock")
	defer p.tracer.end("parseBlock")

	// precondition & post condition
	p.assert(p.currentIs(token.LBrace), "parseBlock must be called with '{' as the current token")
	defer func() {
		p.assert(p.currentIs(token.RBrace), "parseBlock must return with '}' as the current token")
	}()

	block := new(ast.Block)
	block.SetPosition(p.current.Position)
	p.advance()

	for !p.currentIs(token.RBrace) {
		// parse the statement
		block.Statements = append(block.Statements, p.parseStatement())

		// TODO: improvement: Currently, all statements expect func decl require a semicolon.
		//       This is not very elegant due to the verbosity of the syntax.
		//       These should require one:
		//       	*ast.VarDeclaration
		//         	*ast.ConstDeclaration
		// 			*ast.ReturnStatement
		// 			*ast.Assignment
		// 			*ast.ExpressionStatement
		// 		 These shouldn't:
		// 			*ast.ForStatement
		// 			*ast.IfStatement
		// 			*ast.WhileStatement

		// ensure the statement is terminated with a semicolon
		if !p.nextIs(token.Semicolon) {
			p.syntaxError(fmt.Sprintf("expected semicolon after statement, got %s", p.current.Type))
		}
		// after parsing the statement resides on its last token, consume it
		p.advance()
		// consume the semicolon
		p.advance()
	}

	return block
}

// parseStatement parses a Statement.
// ast.FuncDeclaration is explicitly not allowed in this context.
//
//	 Statement = ( VarDeclaration
//				   | ConstDeclaration
//		           | Assignment
//				   | IfStatement
//				   | WhileStatement
//				   | ForStatement
//		           | ReturnStatement
//		           | ExpressionStatement ) .
func (p *parser) parseStatement() ast.Statement {
	p.tracer.begin("parseStatement")
	defer p.tracer.end("parseStatement")

	switch p.current.Type {
	case token.Var:
		return p.parseVarDeclaration()
	case token.Const:
		return p.parseConstDeclaration()
	case token.If:
		return p.parseIfStatement()
	case token.While:
		return p.parseWhileStatement()
	case token.For:
		return convertToWhile(p.parseForStatement())
	case token.Return:
		return p.parseReturnStatement()
	default:
		// an expression must follow in this case
		pos := p.position()
		expr := p.parseExpression(Lowest)

		if slices.Contains(token.AssignmentTypes, p.next.Type) {
			p.advance()
			return p.parseAssignment(expr)
		}

		// fallback to an expression statement
		// currently, only call expressions are allowed
		// as expression statement
		if _, ok := expr.(*ast.CallExpression); !ok {
			p.syntaxErrorAt(pos, fmt.Sprintf(
				"unexpected expressions type: only call expressions are supported as statements, got %s",
				p.current.Type,
			))
		}
		exprStmt := new(ast.ExpressionStatement)
		exprStmt.SetPosition(p.current.Position)
		exprStmt.Expression = expr
		return exprStmt
	}
}

// SimpleStatement = ( VarDeclaration | Assignment ) .
func (p *parser) parseSimpleStatement() ast.SimpleStatement {
	p.tracer.begin("parseSimpleStatement")
	defer p.tracer.end("parseSimpleStatement")

	var s ast.SimpleStatement

	if p.currentIs(token.Var) {
		s = p.parseVarDeclaration()
	} else {
		expr := p.parseExpression(Lowest)
		p.advance()

		s = p.parseAssignment(expr)
	}

	return s
}

// VarDeclaration = "var" identifier TypeID [ "=" Expression ] .
func (p *parser) parseVarDeclaration() *ast.VarDeclaration {
	p.tracer.begin("parseVarDeclaration")
	defer p.tracer.end("parseVarDeclaration")

	// precondition
	p.assert(p.currentIs(token.Var), "parseVarDeclaration must be called with 'var' as the current token")

	v := new(ast.VarDeclaration)
	v.SetPosition(p.current.Position)

	// expect identifier
	if p.advance(); !p.currentIs(token.Identifier) {
		p.syntaxError(fmt.Sprintf("expected identifier in var declaration, got %s", p.current.Literal))
	}

	// parse identifier
	id := new(ast.Identifier)
	id.Name = p.current.Literal
	id.SetPosition(p.current.Position)
	v.Identifier = id
	p.advance()

	// parse the type of the variable declaration
	v.TypeName = p.parseType()

	// no initialization in var declaration, statement ends here
	if p.nextIs(token.Semicolon) {
		return v
	}

	// finish consuming the last token
	p.advance()

	// expect and consume the assignment token
	if !p.currentIs(token.Assign) {
		p.syntaxError(fmt.Sprintf("expected '=' in var declaration, got %s", p.current.Literal))
	}
	p.advance()

	// parse the initialization expression
	v.Expression = p.parseExpression(Lowest)

	return v
}

// ConstDeclaration = "const" identifier TypeID "=" Expression .
func (p *parser) parseConstDeclaration() *ast.ConstDeclaration {
	p.tracer.begin("parseConstDeclaration")
	defer p.tracer.end("parseConstDeclaration")

	// precondition
	p.assert(p.currentIs(token.Const), "parseVarDeclaration must be called with 'const' as the current token")

	c := new(ast.ConstDeclaration)
	c.SetPosition(p.current.Position)

	// expect identifier
	if p.advance(); !p.currentIs(token.Identifier) {
		p.syntaxError(fmt.Sprintf("expected identifier in const declaration, got %s", p.current.Literal))
	}

	// parse the identifier
	id := new(ast.Identifier)
	id.Name = p.current.Literal
	id.SetPosition(p.current.Position)
	c.Identifier = id
	p.advance()

	// parse the type
	c.TypeName = p.parseType()
	p.advance()

	// expect and consume the assignment token
	if !p.currentIs(token.Assign) {
		p.syntaxError(fmt.Sprintf("expected '=' in const declaration, got %s", p.current.Literal))
	}
	p.advance()

	// parse the const expression
	c.Expression = p.parseConstExpression()

	return c
}

// FuncDeclaration  = "fn" identifier FuncSignature Block .
// FuncSignature    = "(" [ FuncParamList ] ")" FuncResult .
// FuncResult       = [ TypeIdentifier ] .
// FuncParamList    = FuncParamItem { "," FuncParamItem } .
// FuncParamItem    = identifier TypeIdentifier .
func (p *parser) parseFunctionDeclaration() *ast.FuncDeclaration {
	p.tracer.begin("parseFunctionDeclaration")
	defer p.tracer.end("parseFunctionDeclaration")

	// precondition
	p.assert(p.currentIs(token.Function), "parseFunctionDeclaration must be called with 'fn' as the current token")

	fn := new(ast.FuncDeclaration)
	fn.SetPosition(p.current.Position)

	// consume the function token
	p.advance()

	// parse the function name
	// The type p.assertion is safe in this case because parseIdentifier
	// produces a value of the underlying type *ast.Identifier.
	fn.Identifier = p.parseIdentifier().(*ast.Identifier)
	p.advance()

	// expect function parameters after name
	if !p.currentIs(token.LParen) {
		p.syntaxError(fmt.Sprintf("expected '(' in function declaration, got %s", p.current.Literal))
	}
	p.advance()

	// parse parameter list
	p.parseElementList(token.Comma, token.RParen, func() {
		param := new(ast.Param)
		param.SetPosition(p.current.Position)
		param.Identifier = p.parseIdentifier().(*ast.Identifier)
		p.advance()
		param.TypeID = p.parseType()
		fn.Parameters = append(fn.Parameters, param)
		p.advance()
	})
	// consume the closing parenthesis
	p.advance()

	// function has no result
	if p.currentIs(token.LBrace) {
		fn.Body = p.parseBlock()
		return fn
	}

	// TODO: improvement: support multi-return valued functions?

	// function has single result type
	returnType := p.parseType()
	fn.Result = returnType
	p.advance()

	if !p.currentIs(token.LBrace) {
		p.syntaxError(fmt.Sprintf("expected start of function body, got %s", p.current.Literal))
	}

	fn.Body = p.parseBlock()

	return fn
}

// ReturnStatement = "return" [ Expression ] .
func (p *parser) parseReturnStatement() *ast.ReturnStatement {
	p.tracer.begin("parseReturnStatement")
	defer p.tracer.end("parseReturnStatement")

	pos := p.current.Position

	ret := new(ast.ReturnStatement)
	ret.SetPosition(pos)

	// a semicolon that no expression is present after the return keyword
	if p.nextIs(token.Semicolon) {
		return ret
	}

	p.advance()

	ret.Expression = p.parseExpression(Lowest)
	return ret
}

// Assignment = ( identifier | IndexExpression ) assign_op Expression .
func (p *parser) parseAssignment(expr ast.Expression) *ast.Assignment {
	p.tracer.begin("parseAssignment")
	defer p.tracer.end("parseAssignment")

	// the left hand side of an assignment can only be an identifier
	// or and index expression
	switch expr.(type) {
	case *ast.Identifier:
	case *ast.IndexExpression:
	default:
		p.syntaxError(fmt.Sprintf(
			"cannot assign to expression '%v', only identifiers or index expressions are supported",
			expr,
		))
	}

	pos := p.current.Position

	// detect assignment if the current token is "="
	if p.current.Type == token.Assign {
		p.advance()

		valueExpr := p.parseExpression(Lowest)

		assignment := new(ast.Assignment)
		assignment.SetPosition(pos)
		assignment.Left = expr
		assignment.Value = valueExpr

		return assignment
	}

	// detect compound assignment if the current token is a compound assignment token (e.g. "+=" or similar)
	if slices.Contains(token.CompoundAssignmentTypes, p.current.Type) {
		assignmentType := p.current.Type

		p.advance()

		valueExpr := p.parseExpression(Lowest)

		op, ok := token.CompoundAssignmentOp[assignmentType]
		p.assert(ok, "missing compound assignment operator")

		assignment := new(ast.Assignment)
		assignment.SetPosition(pos)
		assignment.Left = expr

		// expand the compound assignment (e.g. a += 1 becomes a = a + 1)
		assignment.Value = &ast.InfixExpression{
			Operator: string(op),
			Left:     expr,
			Right:    valueExpr,
		}

		return assignment
	}

	panic("unsupported assignment type")
}

// IfStatement = "if" Expression Block [ "else" ( IfStatement | Block ) ] .
func (p *parser) parseIfStatement() *ast.IfStatement {
	p.tracer.begin("parseIfStatement")
	defer p.tracer.end("parseIfStatement")

	// precondition & post condition
	p.assert(p.currentIs(token.If), "parseIfStatement must be called with 'if' as the current token")
	defer func() {
		p.assert(p.currentIs(token.RBrace), "parseIfStatement must return with '}' as the current token")
	}()

	stmt := new(ast.IfStatement)
	stmt.SetPosition(p.current.Position)
	p.advance()

	// parse the expression of the statement
	// if ... { ...
	//    ^
	stmt.Condition = p.parseExpression(Lowest)
	p.advance()

	// parse the block of the "if" statement
	// if ... { ...
	//        ^
	if !p.currentIs(token.LBrace) {
		p.syntaxError(fmt.Sprintf("expected '{' in if statement, got %s", p.current.Literal))
	}

	// if ... { ... } ...
	//              ^
	stmt.Consequence = p.parseBlock()

	// when there is no else it's a simple "if ... { ... }" statement
	if !p.nextIs(token.Else) {
		return stmt
	}

	// consume the { and the else
	p.advance()
	p.advance()

	// if ... { ... } else { ...
	//                     ^
	if p.currentIs(token.LBrace) {
		stmt.Alternative = p.parseBlock()
		return stmt
	}

	// if ... { ... } else if ...
	//                     ^
	if p.currentIs(token.If) {
		stmt.Alternative = p.parseIfStatement()
	}

	return stmt
}

// WhileStatement = "while" Expression Block .
func (p *parser) parseWhileStatement() *ast.WhileStatement {
	p.tracer.begin("parseWhileStatement")
	defer p.tracer.end("parseWhileStatement")

	// precondition & post condition
	p.assert(p.currentIs(token.While), "parseWhileStatement must be called with 'if' as the current token")
	defer func() {
		p.assert(p.currentIs(token.RBrace), "parseWhileStatement must return with '}' as the current token")
	}()

	// parse the while token
	stmt := new(ast.WhileStatement)
	stmt.SetPosition(p.current.Position)
	p.advance()

	// parse the condition expression
	stmt.Condition = p.parseExpression(Lowest)
	p.advance()

	// parse the body
	stmt.Body = p.parseBlock()

	return stmt
}

// ForStatement := "for" [ ForPre ] ";" ForCondition ";" [ ForPost ] .
// ForCondition := Expression .
// ForPre       := SimpleStatement .
// ForPost      := SimpleStatement .
func (p *parser) parseForStatement() *ast.ForStatement {
	p.tracer.begin("parseForStatement")
	defer p.tracer.end("parseForStatement")

	// precondition & post condition
	p.assert(p.currentIs(token.For), "parseForStatement must be called with 'for' as the current token")
	defer func() {
		p.assert(p.currentIs(token.RBrace), "parseForStatement must return with '}' as the current token")
	}()

	// parse the for token
	stmt := new(ast.ForStatement)
	stmt.SetPosition(p.current.Position)
	p.advance()

	// parse the optional "pre" component
	if !p.currentIs(token.Semicolon) {
		stmt.Pre = p.parseSimpleStatement()
		p.advance()
	}

	// expect a semicolon after pre
	if !p.currentIs(token.Semicolon) {
		p.syntaxError(fmt.Sprintf("expected ';' in for statement, got %s", p.current.Literal))
	}
	p.advance()

	// parse the condition expression
	stmt.Condition = p.parseExpression(Lowest)
	p.advance()

	// ";" after condition is required
	if !p.currentIs(token.Semicolon) {
		p.syntaxError(fmt.Sprintf("expected ';' in for statement, got %s", p.current.Literal))
	}
	p.advance()

	// parse the optional "post" component if the block doesn't start yet
	if !p.currentIs(token.LBrace) {
		stmt.Post = p.parseSimpleStatement()
		p.advance()
	}

	stmt.Body = p.parseBlock()
	return stmt
}

// parseExpression is the entrypoint for parsing an Expression.
func (p *parser) parseExpression(rbp int) ast.Expression {
	p.tracer.begin("parseExpression")
	defer p.tracer.end("parseExpression")

	parsePrefixExpr := p.prefix(p.current.Type)
	if parsePrefixExpr == nil {
		p.syntaxError(fmt.Sprintf("failed to parse expression at '%s': no prefix parse function found", p.current.Literal))
		return nil
	}

	left := parsePrefixExpr()

	for !p.nextIs(token.Semicolon) && rbp < rbpOf(p.next.Type) {
		parseInfixExpr := p.infix(p.next.Type)
		if parseInfixExpr == nil {
			return left
		}

		p.advance()
		left = parseInfixExpr(left)
	}

	return left
}

// IndexExpression = Expression "[" Expression "]" .
func (p *parser) parseIndexExpression(left ast.Expression) ast.Expression {
	p.tracer.begin("parseIndexExpression")
	defer p.tracer.end("parseIndexExpression")

	ie := new(ast.IndexExpression)
	ie.SetPosition(p.current.Position)
	ie.Left = left
	p.advance()

	ie.Index = p.parseExpression(Lowest)
	p.advance()

	if !p.currentIs(token.RBracket) {
		p.syntaxError(fmt.Sprintf("expected ']' in index expression, got %s", p.current.Literal))
	}

	return ie
}

// parseInfixExpression parses and returns an ast.InfixExpression
func (p *parser) parseInfixExpression(left ast.Expression) ast.Expression {
	p.tracer.begin("parseInfixExpression")
	defer p.tracer.end("parseInfixExpression")

	expr := new(ast.InfixExpression)
	expr.SetPosition(p.current.Position)
	expr.Operator = p.current.Literal
	expr.Left = left

	currentRBP := rbpOf(p.current.Type)
	p.advance()

	expr.Right = p.parseExpression(currentRBP)

	// This is an explicitly defined limitation, because there is no currently
	// operation which is possible on array literals
	if _, ok := expr.Left.(*ast.ArrayLiteral); ok {
		p.syntaxError("array literals cannot appear in infix expressions")
	}
	if _, ok := expr.Right.(*ast.ArrayLiteral); ok {
		p.syntaxError("array literals cannot appear in infix expressions")
	}

	return expr
}

// CallExpression = Expression "(" [ Expression { "," Expression } ] ")" .
func (p *parser) parseCallExpression(left ast.Expression) ast.Expression {
	p.tracer.begin("parseCallExpression")
	defer p.tracer.end("parseCallExpression")

	// ensure the left side is an identifier
	funcIdent, ok := left.(*ast.Identifier)
	if !ok {
		p.syntaxErrorAt(left.Position(), "left side of function call expression must be an identifier")
	}

	expr := new(ast.CallExpression)
	expr.SetPosition(p.current.Position)
	expr.Function = funcIdent

	// consume the left parenthesis
	p.advance()

	// consume the arguments if there are any
	p.parseElementList(token.Comma, token.RParen, func() {
		argExpr := p.parseExpression(Lowest)
		expr.Arguments = append(expr.Arguments, argExpr)
		p.advance()
	})

	return expr
}

// parsePrefixExpression parses and returns an ast.PrefixExpression
func (p *parser) parsePrefixExpression() ast.Expression {
	p.tracer.begin("parsePrefixExpression")
	defer p.tracer.end("parsePrefixExpression")

	expr := new(ast.PrefixExpression)
	expr.SetPosition(p.current.Position)
	expr.Operator = p.current.Literal

	p.advance()

	expr.Right = p.parseExpression(Unary)

	return expr
}

// parseGroupExpression parses an expression explicitly grouped with parentheses
func (p *parser) parseGroupExpression() ast.Expression {
	p.tracer.begin("parseGroupExpression")
	defer p.tracer.end("parseGroupExpression")

	// precondition
	p.assert(p.currentIs(token.LParen), "parseGroupExpression must be called with '(' as the current token")

	// skip the opening parenthesis
	p.advance()

	expr := p.parseExpression(Lowest)
	p.advance()

	if !p.currentIs(token.RParen) {
		p.syntaxError(fmt.Sprintf("expected closing ')' in group expression, got %s", p.current.Literal))
	}

	return expr
}

// TypeIdentifier = BasicTypeName | ArrayType .
func (p *parser) parseType() ast.TypeIdentifier {
	p.tracer.begin("parseType")
	defer p.tracer.end("parseType")

	// array type always starts with a [
	if p.currentIs(token.LBracket) {
		return p.parseArrayType()
	}

	// otherwise it must be a built-in type
	return p.parseBasicTypeName()
}

// ArrayType := "[" int_literal "]" TypeIdentifier .
func (p *parser) parseArrayType() *ast.ArrayType {
	p.tracer.begin("parseArrayType")
	defer p.tracer.end("parseArrayType")

	// precondition
	p.assert(p.currentIs(token.LBracket), "parseArrayType must be called with '[' as the current token")

	arrType := new(ast.ArrayType)
	arrType.SetPosition(p.current.Position)

	// consume the left bracket
	p.advance()

	// parse the length expression
	arrType.Len = p.parseBasicLiteral().(*ast.BasicLiteral)
	p.advance()

	// expect and consume right bracket
	if !p.currentIs(token.RBracket) {
		p.syntaxError(fmt.Sprintf("expected ']', got %s", p.current.Literal))
	}
	p.advance()

	// parse the type
	arrType.ElementType = p.parseType()

	return arrType
}

// ArrayLiteral = ArrayType "{" Expression { "," Expression } "}" .
func (p *parser) parseArrayLiteral() ast.Expression {
	p.tracer.begin("parseArrayLiteral")
	defer p.tracer.end("parseArrayLiteral")

	// precondition
	p.assert(p.currentIs(token.LBracket), "parseArrayLiteral must be called with '[' as the current token")

	// initialize the literal
	lit := new(ast.ArrayLiteral)
	lit.SetPosition(p.current.Position)

	// parse type
	lit.TypeID = p.parseArrayType()
	p.advance()

	if !p.currentIs(token.LBrace) {
		p.syntaxError(fmt.Sprintf("expected '{', got '%v'", p.current.Literal))
	}

	// consume the opening brace
	p.advance()

	// parse elements
	p.parseElementList(token.Comma, token.RBrace, func() {
		elemExpr := p.parseExpression(Lowest)
		lit.Elements = append(lit.Elements, elemExpr)
		p.advance()
	})

	return lit
}

func (p *parser) parseConversion() ast.Expression {
	p.tracer.begin("parseConversion")
	defer p.tracer.end("parseConversion")

	// T(x)
	callExpr := new(ast.CallExpression)
	callExpr.SetPosition(p.current.Position)
	callExpr.Function = &ast.Identifier{Name: p.current.Literal}

	// consume the left parenthesis
	p.advance()

	argExpr := p.parseExpression(Lowest)
	callExpr.Arguments = append(callExpr.Arguments, argExpr)

	return callExpr
}

func (p *parser) parseIdentifier() ast.Expression {
	p.tracer.begin("parseIdentifier")
	defer p.tracer.end("parseIdentifier")

	// precondition
	p.assert(
		p.currentIs(token.Identifier),
		fmt.Sprintf(
			"parseIdentifier must be called with identifier as the current token, got %v",
			p.current.Literal,
		),
	)

	if !p.currentIs(token.Identifier) {
		p.syntaxError(fmt.Sprintf("expected identifier, got '%v'", p.current.Literal))
	}

	id := new(ast.Identifier)
	id.SetPosition(p.current.Position)
	id.Name = p.current.Literal

	return id
}

func (p *parser) parseBasicTypeName() *ast.BasicTypeName {
	p.tracer.begin("parseTypeIdentifier")
	defer p.tracer.end("parseTypeIdentifier")

	// precondition
	p.assert(
		p.currentIs(token.Identifier) ||
			p.currentIs(token.Int) ||
			p.currentIs(token.Bool) ||
			p.currentIs(token.Byte) ||
			p.currentIs(token.String),
		fmt.Sprintf(
			"parseIdentifier must be called with identifier as the current token, got %v",
			p.current.Literal,
		),
	)

	id := new(ast.BasicTypeName)
	id.SetPosition(p.current.Position)
	id.Name = p.current.Literal

	builtin := []token.Type{token.Int, token.Bool, token.String, token.Byte}
	if slices.Contains(builtin, p.current.Type) {
		return id
	}

	if !p.currentIs(token.Identifier) {
		p.syntaxError(fmt.Sprintf("expected identifier, got '%v'", p.current.Literal))
	}

	return id
}

func (p *parser) parseBasicLiteral() ast.Expression {
	p.tracer.begin("parseBasicLiteral")
	defer p.tracer.end("parseBasicLiteral")

	// precondition
	p.assert(
		p.currentIs(token.IntegerLit) ||
			p.currentIs(token.True) ||
			p.currentIs(token.False) ||
			p.currentIs(token.StringLit),
		"parseBasicLiteral must be called with a built-in type as the current token",
	)

	lit := new(ast.BasicLiteral)
	lit.SetPosition(p.current.Position)
	lit.SetValue(constant.FromLiteral(p.current))

	return lit
}

// ConstExpression = int_literal | bool_literal | string_literal .
func (p *parser) parseConstExpression() ast.ConstExpression {
	p.tracer.begin("parseConstExpression")
	defer p.tracer.end("parseConstExpression")

	// this type p.assertion is safe because
	// parseIntLiteral returns an ast.BasicLiteral under
	// the hood, which is an ast.ConstExpression
	return p.parseBasicLiteral().(ast.ConstExpression)
}

func (p *parser) currentIs(t token.Type) bool {
	return p.current.Type == t
}

func (p *parser) nextIs(t token.Type) bool {
	return p.next.Type == t
}

func (p *parser) nextIn(ts ...token.Type) bool {
	for _, t := range ts {
		if p.nextIs(t) {
			return true
		}
	}
	return false
}

// advance advances the current token and populates the next token using the TokenSource.
func (p *parser) advance() {
	p.current = p.next
	p.next = p.tokens.Next()
}

func (p *parser) parseElementList(
	separator, terminator token.Type,
	f func(),
) {
	for !p.currentIs(terminator) && !p.currentIs(token.EOF) {
		f()

		// no trailing comma
		if p.currentIs(terminator) {
			break
		}

		// skip trailing comma
		if p.currentIs(separator) && p.nextIs(terminator) {
			p.advance()
			break
		}

		// separator must exist between elements
		if !p.currentIs(separator) {
			p.syntaxError(fmt.Sprintf("expected '%s', got '%s'", separator, p.current.Type))
		}

		p.advance()
	}
}

func (p *parser) position() token.Position {
	return p.current.Position
}

func (p *parser) syntaxError(message string) {
	p.syntaxErrorAt(p.position(), message)
}

func (p *parser) syntaxErrorAt(pos token.Position, message string) {
	p.errHandler(fmt.Errorf("%d:%d: syntax error: %s", pos.Row, pos.Col, message))
}

func (p *parser) assert(condition bool, msg string) {
	if !p.debug {
		return
	}
	if condition {
		return
	}
	panic(msg)
}

// convertToWhile converts the given [ast.ForStatement] to an equivalent [ast.WhileStatement].
// This is implemented to show equivalence of for and while.
// Additionally, it will make code generation a tad bit simpler as no code-generation logic
// for the [ast.ForStatement] needs to be implemented.
// The returned value is an [ast.Block] which contains the
// ForPre component followed by the created [ast.WhileStatement].
func convertToWhile(f *ast.ForStatement) ast.Statement {
	// create a new "shadow" block which doesn't really exist in the parsed source
	block := new(ast.Block)

	// the for pre component is executed once before the loop
	if f.Pre != nil {
		block.Statements = append(block.Statements, f.Pre)
	}

	// the condition and body are equivalent to a while loop, so it can simply be re-assigned
	while := new(ast.WhileStatement)
	while.Condition = f.Condition
	while.Body = f.Body

	// the for post component is executed at the end of the block
	if f.Post != nil {
		while.Body.Statements = append(while.Body.Statements, f.Post)
	}

	// append the converted statement to the block
	block.Statements = append(block.Statements, while)
	return block
}
