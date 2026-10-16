grammar Lola ;


// Parser

lola: statement+ EOF ;

statement: (input | internal | comp | definition | assignment | defAssign | output | enumDef | linked) SEMI ;
input: IN id COLON exprType ;
internal: INTERNAL id COLON exprType ;
comp: COMP id ;
definition: DEF id COLON exprType ;
assignment: id ASSIGN expr ;
defAssign: DEF id COLON exprType ASSIGN expr ;
output: OUT id (FROM number TO number BY number)? ;
enumDef: ENUM ENUM_TYPE LBRACE enumConst (COMMA enumConst)* RBRACE ;
linked: id LINKED WITH id ;

id: IDENTIFIER ;
exprType
  : BOOL      # boolType
  | REAL      # realType
  | ENUM_TYPE # enumType
  ;
boolLiteral
  : TRUE    # trueLiteral
  | FALSE   # falseLiteral
  ;
number: INT | FLOAT ;
enumConst: ENUM_CONST ;
constant: boolLiteral | number | enumConst;

prefixOp: NOT | MINUS;

// precedence classes of infix operators
infixOpPrio1: TIMES ;
infixOpPrio2: PLUS | MINUS ;
infixOpPrio3: EQ | NEQ | LT | GT | LEQ | GEQ ;
infixOpPrio4: AND | OR ;
infixOpPrio5: IMPL | IFF ;

expr
  : prefixOp expr                           # prefixOpExpr
  | expr infixOpPrio1 expr                  # infixOp1Expr
  | expr infixOpPrio2 expr                  # infixOp2Expr
  | expr infixOpPrio3 expr                  # infixOp3Expr
  | expr infixOpPrio4 expr                  # infixOp4Expr
  | expr infixOpPrio5 expr                  # infixOp5Expr
  | IF expr THEN expr ELSE expr             # iteExpr
  | id LBRACKET INT COMMA constant RBRACKET # offsetExpr
  | constant                                # constantExpr
  | id                                      # idExpr
  | LPAR expr RPAR                          # parExpr
  ;


// Lexer

EQ: '==' ;
NEQ: '!=' ;
NOT: '!' ;
AND: '&&' ;
OR: '||' ;
IFF: '<->' ;
IMPL: '->' ;
PLUS: '+' ;
MINUS: '-' ;
TIMES: '*' ;
LT: '<' ;
GT: '>' ;
LEQ: '<=' ;
GEQ: '>=' ;

ASSIGN: '=' ;
DEF: 'def' ;
IN: 'in' ;
OUT: 'out' ;
INTERNAL: 'internal' ;
ENUM: 'enum' ;
IF: 'if' ;
THEN: 'then' ;
ELSE: 'else' ;
COLON: ':' ;
SEMI: ';' ;
DOT: '.' ;
COMMA: ',' ;
LBRACKET: '[' ;
RBRACKET: ']' ;
LPAR: '(' ;
RPAR: ')' ;
LBRACE: '{' ;
RBRACE: '}' ;
FROM: 'from' ;
TO: 'to' ;
BY: 'by' ;
LINKED: 'linked' ;
WITH: 'with' ;
COMP: 'comp' ;

BOOL: 'Bool' ;
REAL: 'Real' ;

TRUE: 'true' ;
FALSE: 'false' ; 

fragment CHAR: [a-zA-Z0-9_] ;
IDENTIFIER: [a-z_] CHAR* | 'ASSUMPTION' ;
ENUM_CONST: [A-Z] [A-Z0-9_]* ;
ENUM_TYPE: [A-Z] CHAR+ ;

fragment NUMERIC: [0-9]+ ;
INT: (PLUS | MINUS)? NUMERIC ;
FLOAT: INT DOT NUMERIC ;

LINE_COMMENT: '%' ~[\r\n]* -> skip ;

WHITESPACE: (' ' | '\t')+ -> skip ;
NEWLINE: ('\r'? '\n' | '\r')+ -> skip ;

// Comment this line to not hand over lexer errors to the parser.
// The resulting lexer error messages may be a bit more precise 
// but if the LolaCollectingErrorListener is used the parser error messages afterwards may be confusing.
ERR_CHAR : . ; 