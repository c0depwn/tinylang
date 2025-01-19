package lexer

import (
	"github.com/c0depwn/tinylang/token"
)

// The following is a finite-state-machine used to detect tokens
// from the input source.
// The current state and the next character which is read
// is used to transition to the next using the transition functions.
//
// The state transitions are stored in the transitions array and initialized
// using the initTransitionTable function.
//
// The state resulting from the transition can be one of the following:
// - 0, which means that the subsequent state is invalid
// - a valid lexerState, which potentially results in a token.Token
//
// A non-zero state
// A non-zero state is used to signal that the state transition is valid

type lexerState = uint

const (
	start lexerState = iota

	stateAssign
	stateNot

	stateMul
	stateDiv
	stateMod
	stateSum
	stateSub
	stateBitshiftL
	stateBitshiftR
	stateBitwiseAnd
	stateBitwiseOr
	stateBitwiseXor
	stateLogicAnd
	stateLogicOr

	stateEq
	stateNotEq
	stateGreaterThan
	stateGreaterThanEq
	stateLessThan
	stateLessThanEq

	stateComma
	stateSemicolon
	stateLParen
	stateRParen
	stateLBrace
	stateRBrace
	stateColon
	stateLBracket
	stateRBracket
	stateBacktick

	stateAssignMul
	stateAssignDiv
	stateAssignMod
	stateAssignAdd
	stateAssignSub
	stateAssignShiftL
	stateAssignShiftR
	stateAssignAnd
	stateAssignOr
	stateAssignXor

	stateIdentifier
	stateIntHexLiteralStart
	stateIntHexLiteral
	stateIntBinLiteralStart
	stateIntBinLiteral
	stateIntDecLiteral
	statePartialStringLiteral
	stateStringLiteral

	stateComment
)

// MaxStates stores the maximum number of instances of lexerState
const (
	MaxStates = 64
	MaxChars  = 1 << 8
)

type (
	transitionTable = [MaxStates][MaxChars]uint
	stateTokenType  = [MaxStates]token.Type
)

var (
	// transitions stores the state transitions for the finite-state-machine
	// which is used to recognize tokens.
	transitions = initTransitionTable()
	// stateTokenTypeMap maps a lexerState to a token.Type
	stateTokenTypeMap = initStateTokenType()
)

// initTransitionTable initializes the state transition table
// for the finite-state-machine used to recognize tokens.
func initTransitionTable() transitionTable {
	// int[MAX_STATES][MAX_CHARS]
	transitions := transitionTable{}

	registerIdentifier(&transitions)
	registerIntLiteral(&transitions)
	registerStringLiteral(&transitions)
	registerComment(&transitions)

	transitions[start]['='] = stateAssign
	transitions[start]['!'] = stateNot
	transitions[start]['*'] = stateMul
	transitions[start]['/'] = stateDiv
	transitions[start]['%'] = stateMod
	transitions[start]['+'] = stateSum
	transitions[start]['-'] = stateSub
	transitions[start]['<'] = stateLessThan
	transitions[start]['>'] = stateGreaterThan
	transitions[start]['&'] = stateBitwiseAnd
	transitions[start]['|'] = stateBitwiseOr
	transitions[start]['^'] = stateBitwiseXor
	transitions[start][','] = stateComma
	transitions[start][';'] = stateSemicolon
	transitions[start]['('] = stateLParen
	transitions[start][')'] = stateRParen
	transitions[start]['{'] = stateLBrace
	transitions[start]['}'] = stateRBrace
	transitions[start][':'] = stateColon
	transitions[start]['['] = stateLBracket
	transitions[start][']'] = stateRBracket

	transitions[stateMul]['='] = stateAssignMul
	transitions[stateDiv]['='] = stateAssignDiv
	transitions[stateMod]['='] = stateAssignMod
	transitions[stateSum]['='] = stateAssignAdd
	transitions[stateSub]['='] = stateAssignSub
	transitions[stateBitwiseAnd]['='] = stateAssignAnd
	transitions[stateBitwiseOr]['='] = stateAssignOr
	transitions[stateBitwiseXor]['='] = stateAssignXor
	transitions[stateLessThan]['<'] = stateBitshiftL
	transitions[stateGreaterThan]['>'] = stateBitshiftR
	transitions[stateBitwiseAnd]['&'] = stateLogicAnd
	transitions[stateBitwiseOr]['|'] = stateLogicOr

	transitions[stateAssign]['='] = stateEq
	transitions[stateNot]['='] = stateNotEq
	transitions[stateLessThan]['='] = stateLessThanEq
	transitions[stateGreaterThan]['='] = stateGreaterThanEq
	transitions[stateBitshiftL]['='] = stateAssignShiftL
	transitions[stateBitshiftR]['='] = stateAssignShiftR

	return transitions
}

func initStateTokenType() stateTokenType {
	m := stateTokenType{}

	// control
	m[0] = token.EOF

	// id
	m[stateIdentifier] = token.Identifier

	// literals & identifiers
	m[stateIntBinLiteral] = token.IntegerLit
	m[stateIntHexLiteral] = token.IntegerLit
	m[stateIntDecLiteral] = token.IntegerLit
	m[stateStringLiteral] = token.StringLit

	// operators
	m[stateNot] = token.Excl
	m[stateMul] = token.Mul
	m[stateDiv] = token.Div
	m[stateMod] = token.Mod
	m[stateSum] = token.Sum
	m[stateSub] = token.Sub
	m[stateBitshiftL] = token.BitShiftL
	m[stateBitshiftR] = token.BitShiftR
	m[stateBitwiseAnd] = token.BitwiseAnd
	m[stateBitwiseOr] = token.BitwiseOr
	m[stateBitwiseXor] = token.BitwiseXor
	m[stateLogicAnd] = token.LogicalAnd
	m[stateLogicOr] = token.LogicalOr

	// relational
	m[stateLessThan] = token.LessThan
	m[stateGreaterThan] = token.GreaterThan
	m[stateEq] = token.Equal
	m[stateLessThanEq] = token.LessThanEqual
	m[stateGreaterThanEq] = token.GreaterThanEqual
	m[stateNotEq] = token.NotEqual

	// assignments
	m[stateAssign] = token.Assign
	m[stateAssignMul] = token.AssignMul
	m[stateAssignDiv] = token.AssignDiv
	m[stateAssignMod] = token.AssignMod
	m[stateAssignAdd] = token.AssignAdd
	m[stateAssignSub] = token.AssignSub
	m[stateAssignShiftL] = token.AssignShiftL
	m[stateAssignShiftR] = token.AssignShiftR
	m[stateAssignAnd] = token.AssignBitAnd
	m[stateAssignOr] = token.AssignBitOr
	m[stateAssignXor] = token.AssignBitXor

	// delimiters
	m[stateComma] = token.Comma
	m[stateSemicolon] = token.Semicolon
	m[stateLParen] = token.LParen
	m[stateRParen] = token.RParen
	m[stateLBrace] = token.LBrace
	m[stateRBrace] = token.RBrace
	m[stateLBracket] = token.LBracket
	m[stateRBracket] = token.RBracket

	m[stateComment] = token.Comment

	return m
}

func fromState(
	state lexerState,
	literal string,
	r, c int,
) token.Token {
	tokenType := stateTokenTypeMap[state]

	// some identifiers are keywords
	if tokenType == token.Identifier {
		tokenType = token.LookupIdentifier(literal)
	}

	return token.Token{
		Type:     tokenType,
		Literal:  literal,
		Position: token.Position{Row: r, Col: c},
	}
}

func registerIdentifier(transitions *transitionTable) {
	// identifiers have the form /([a-zA-Z_])([a-zA-Z0-9_])*/
	for i := 'a'; i <= 'z'; i++ {
		transitions[start][i] = stateIdentifier
		transitions[stateIdentifier][i] = stateIdentifier
	}

	for i := 'A'; i <= 'Z'; i++ {
		transitions[start][i] = stateIdentifier
		transitions[stateIdentifier][i] = stateIdentifier
	}

	for i := '0'; i <= '9'; i++ {
		transitions[stateIdentifier][i] = stateIdentifier
	}

	transitions[start]['_'] = stateIdentifier
	transitions[stateIdentifier]['_'] = stateIdentifier
}

// TODO: bug, zero is special
func registerIntLiteral(transitions *transitionTable) {
	// decimal integer literals have the form /([1-9])([0-9])*/
	for i := '0'; i <= '9'; i++ {
		transitions[start][i] = stateIntDecLiteral
	}

	// stateIntDeclLiteral[x] = 0x -> hex begin
	transitions[stateIntDecLiteral]['x'] = stateIntHexLiteralStart
	registerHexIntLiteral(transitions)

	// stateIntDeclLiteral[b] = 0b -> bin begin
	transitions[stateIntDecLiteral]['b'] = stateIntBinLiteralStart
	registerBinIntLiteral(transitions)

	for i := '0'; i <= '9'; i++ {
		transitions[stateIntDecLiteral][i] = stateIntDecLiteral
	}
}

func registerHexIntLiteral(transitions *transitionTable) {
	// hexadecimal literals have the form /0x([A-Fa-f0-9])+/
	// the 0x part is handled by the int literal registration

	for i := '0'; i <= '9'; i++ {
		transitions[stateIntHexLiteralStart][i] = stateIntHexLiteral
		transitions[stateIntHexLiteral][i] = stateIntHexLiteral
	}
	for i := 'a'; i <= 'f'; i++ {
		transitions[stateIntHexLiteralStart][i] = stateIntHexLiteral
		transitions[stateIntHexLiteral][i] = stateIntHexLiteral
	}
	for i := 'A'; i <= 'F'; i++ {
		transitions[stateIntHexLiteralStart][i] = stateIntHexLiteral
		transitions[stateIntHexLiteral][i] = stateIntHexLiteral
	}
}

func registerBinIntLiteral(transitions *transitionTable) {
	// binary literals have the form /0b([01])+/
	// the 0b part is handled by the int literal registration
	transitions[stateIntBinLiteralStart]['0'] = stateIntBinLiteral
	transitions[stateIntBinLiteralStart]['1'] = stateIntBinLiteral
	transitions[stateIntBinLiteral]['0'] = stateIntBinLiteral
	transitions[stateIntBinLiteral]['1'] = stateIntBinLiteral
}

// currently, only raw string literals are supported which
// will not be interpreted (e.g. no escaping using \, \n will not be a newline, ...)
func registerStringLiteral(transitions *transitionTable) {
	// begin with a double quote
	transitions[start]['`'] = stateBacktick

	// any character (ascii)
	for i := 0; i <= 255; i++ {
		transitions[stateBacktick][i] = statePartialStringLiteral
		transitions[statePartialStringLiteral][i] = statePartialStringLiteral
	}

	// end with a double quote
	transitions[stateBacktick]['`'] = stateStringLiteral
	transitions[statePartialStringLiteral]['`'] = stateStringLiteral
}

func registerComment(transitions *transitionTable) {
	// "//" is the comment delimiter
	transitions[stateDiv]['/'] = stateComment
	// any character can be in a comment
	for i := 0; i <= 127; i++ {
		transitions[stateComment][i] = stateComment
	}
	// a newline indicates the end of a comment
	transitions[stateComment]['\n'] = 0
}
