
## Notation

```text
-   exception
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
.	marks the end of a production rule
#   comment
/Regex/  regex

Format of a production
<ProductionName>    ::= <Production Rule> .
```

## Source Code

Source code is UTF-8 encoded Unicode although only ASCII is accepted. 

### Characters

```text
char_whitespace      = ( " " | "\t" | "\n" ) .
char_digit           = /[0-9]/ .
char_letter          = /[a-zA-Z]/ .
char_ascii           = /[\x00-\x7F]/ .
```

### Comments

TODO: 
- //
- /**/

### Identifiers

Identifiers define or reference program entities such as variables and types.

```text
identifier          := char_letter { ( char_letter | char_digit | "_" ) } .
```

### Keywords

```text
TBD
```

### Numeric Literals

Integers can be specified in decimal, binary or hexadecimal form.

```text
bin_digit           := ( "0" | "1" ) .
dec_digit           := /[0-9]/ .
hex_digit           := /[A-Fa-f0-9]/ .

bin_literal         := "0b" bin_digit { bin_digit } .
dec_literal         := dec_digit { dec_digit } .
hex_literal         := "0x" hex_digit { hex_digit } .

int_literal         := ( bin_literal | dec_literal | hex_literal ) .
```

### Boolean Literals

```text
bool_literal        := ( "true" | "false" )
```

### String Literals

```text
string_lit              ::= raw_string_lit | interpreted_string_lit
raw_string_lit          ::= "'" { char_ascii } "'"
interpreted_string_lit  ::= // WE WON'T DO THESE
```

### Composed Literal

```text
ComposedLiteral     ::= LiteralType LiteralValue .
LiteralType         ::= ArrayType
LiteralValue        ::= "{" [ ElementList [ "," ] ] "}"
ElementList         ::= Element { "," Element } .
Element             ::= Expression | LiteralValue
```

## Types

```text
Type        ::= identifier | TypeLiteral
TypeLiteral ::= ArrayType
```

### Strings

Immutable, TODO

### Arrays

Fixed-length & immutable.

```text
ArrayType       ::= "[" ArrayLength "]" ArrayItemType
ArrayLength     ::= Expression . // must evaluate to non-negative int and be constant expression
ArrayItemType   ::= Type .
```

## Operators

The following are the supported operators.

```text
unary_op            := ( "!" | "-" )
product_op          := ( "*" | "/" | "%" )
sum_op              := ( "+" | "-" )
shift_op            := ( "<<" | ">>" )
bit_op              := ( "&" | "|" | "^" )
eq_op               := ( "==" | "<=" | ">=" | "!=" | "<" | ">" )
logic_op            := ( "&&" | "||" )
```

## Block

Blocks contain a sequence of statements.
Currently, an implicit block is defined around the program.


```text
Block               ::= "{" StatementList "}" .
StatementList       ::= { Statement [ ";" ] } . # MAYBE THIS IS A BAD IDEA
```
// TODO:  use the semicolon as terminator between statements and avoid it in the var/decl definition 

## Statements

Statements control the flow of execution.

```text
Statement           ::= ( VarDeclaration
                        | ConstDeclaration 
                        | Assignment
                        | ExpressionStatement ) . # TODO: EXPLICIT RETURN INSTEAD OF EXPRESSION STATEMENT
```

### Variable Declaration

Variables define program entities of a specific type which can be re-assigned.

TODOs:
- Explicit type definition
- Declaration without initialization

```text
VarDeclaration ::= "var" identifier "=" Expression [ ";" ] .      
```

#### Short Variable Declaration

A shorthand for variable declaration where the type is inferred.

```text
ShortVarDeclaration ::= identifier ":=" Expression .
```

### Const Declaration

Constants are program entities of a specific type which must have a constant value and cannot be re-assigned.
There are additional limitations on the initialization of constants: *TBD*

TODOs:
- Explicit type definition
- Limitation on initializers

```text
ConstDeclaration    ::= "const" identifier "=" Expression ";" .
```

### Assignments

Assignments assign the result of an expression to an identifier such as a variable.
Assignments which include an operator in addition to "=" are also referred to as "compound assignments".

```text
assign_op       ::= [ product_op | sum_op | shift_op | bit_op ] "=" .

Assignment      ::= identifier assign_op Expression .
```

### If Statements

```text
IfStatement     := "if" Expression Block [ "else" ( IfStatement | Block ) ] .
```

### While Loop

while E {}
```text
WhileStatement := "while" Expression Block .
```

### For Loop

```text
ForStatement ::= "for" [ ForPre ] ";" ForCondition ";" [ ForPost ] .
ForCondition ::= Expression .
ForPre       ::= SimpleStatement .
ForPost      ::= SimpleStatement .

// TBA
// for idx in array {}
// for idx, value in array {}
// for idx, item in iterator {}
IterClause  ::=  [ ExprList "=" | IdentList ":=" ] "in" Expression .

SimpleStatement ::= VarDeclaration | Assignment .
```

## Expressions

Expressions produce a value through computation.

```text
Expression              := ( LiteralExpression
                           | OperatorExpression ) .

LiteralExpression       ::= ( literal_bool 
                            | literal_int 
                            | literal_char          # TODO
                            | literal_string 
                            | ComposedLiteral ) .    # TODO



OperatorExpression      := ( InfixExpression
                           | PrefixExpression ) .
                       
PrefixExpression        ::= ( "-" | "!" ) Expression .

InfixExpression         ::= ( ArithmeticExpression
                            | BooleanExpression
                            | RelationalExpression ) .

ArithmeticExpression    ::= Expression ( product_op | sum_op | shift_op | bit_op ) Expression .
BooleanExpression       ::= Expression logic_op Expression . 
RelationalExpression    ::= Expression eq_op Expression .
```
