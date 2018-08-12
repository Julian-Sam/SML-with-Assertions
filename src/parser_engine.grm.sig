signature AssertEngine_TOKENS =
sig
type ('a,'b) token
type svalue
val ID: (string) *  'a * 'a -> (svalue,'a) token
val QUOTE_ID: (string) *  'a * 'a -> (svalue,'a) token
val SYMBOLS: (string) *  'a * 'a -> (svalue,'a) token
val ENSURES: (string) *  'a * 'a -> (svalue,'a) token
val REQUIRES: (string) *  'a * 'a -> (svalue,'a) token
val RASSERT: (string) *  'a * 'a -> (svalue,'a) token
val LASSERT: (string) *  'a * 'a -> (svalue,'a) token
val DOTTED_WILDCARD: (string) *  'a * 'a -> (svalue,'a) token
val SHARING: (string) *  'a * 'a -> (svalue,'a) token
val EQTYPE: (string) *  'a * 'a -> (svalue,'a) token
val WHERE: (string) *  'a * 'a -> (svalue,'a) token
val INCLUDE: (string) *  'a * 'a -> (svalue,'a) token
val FUNCTOR: (string) *  'a * 'a -> (svalue,'a) token
val SIGNATURE: (string) *  'a * 'a -> (svalue,'a) token
val SIG: (string) *  'a * 'a -> (svalue,'a) token
val STRUCTURE: (string) *  'a * 'a -> (svalue,'a) token
val STRUCT: (string) *  'a * 'a -> (svalue,'a) token
val NONFIX: (string) *  'a * 'a -> (svalue,'a) token
val INFIXR: (string) *  'a * 'a -> (svalue,'a) token
val INFIX: (string) *  'a * 'a -> (svalue,'a) token
val LOCAL: (string) *  'a * 'a -> (svalue,'a) token
val OPEN: (string) *  'a * 'a -> (svalue,'a) token
val AS: (string) *  'a * 'a -> (svalue,'a) token
val WITHTYPE: (string) *  'a * 'a -> (svalue,'a) token
val WITH: (string) *  'a * 'a -> (svalue,'a) token
val ABSTYPE: (string) *  'a * 'a -> (svalue,'a) token
val DATATYPE: (string) *  'a * 'a -> (svalue,'a) token
val TYPE: (string) *  'a * 'a -> (svalue,'a) token
val REC: (string) *  'a * 'a -> (svalue,'a) token
val OP: (string) *  'a * 'a -> (svalue,'a) token
val WILD: (string) *  'a * 'a -> (svalue,'a) token
val OF: (string) *  'a * 'a -> (svalue,'a) token
val CASE: (string) *  'a * 'a -> (svalue,'a) token
val FUN: (string) *  'a * 'a -> (svalue,'a) token
val FN: (string) *  'a * 'a -> (svalue,'a) token
val AND: (string) *  'a * 'a -> (svalue,'a) token
val VAL: (string) *  'a * 'a -> (svalue,'a) token
val EXCEPTION: (string) *  'a * 'a -> (svalue,'a) token
val RAISE: (string) *  'a * 'a -> (svalue,'a) token
val HANDLE: (string) *  'a * 'a -> (svalue,'a) token
val ANDALSO: (string) *  'a * 'a -> (svalue,'a) token
val ORELSE: (string) *  'a * 'a -> (svalue,'a) token
val END: (string) *  'a * 'a -> (svalue,'a) token
val IN: (string) *  'a * 'a -> (svalue,'a) token
val LET: (string) *  'a * 'a -> (svalue,'a) token
val DO: (string) *  'a * 'a -> (svalue,'a) token
val WHILE: (string) *  'a * 'a -> (svalue,'a) token
val ELSE: (string) *  'a * 'a -> (svalue,'a) token
val THEN: (string) *  'a * 'a -> (svalue,'a) token
val IF: (string) *  'a * 'a -> (svalue,'a) token
val RPAREN: (string) *  'a * 'a -> (svalue,'a) token
val LPAREN: (string) *  'a * 'a -> (svalue,'a) token
val RBRACK: (string) *  'a * 'a -> (svalue,'a) token
val LBRACK: (string) *  'a * 'a -> (svalue,'a) token
val RCURLY: (string) *  'a * 'a -> (svalue,'a) token
val LCURLY: (string) *  'a * 'a -> (svalue,'a) token
val HASH: (string) *  'a * 'a -> (svalue,'a) token
val COLONGT: (string) *  'a * 'a -> (svalue,'a) token
val COLON: (string) *  'a * 'a -> (svalue,'a) token
val BAR: (string) *  'a * 'a -> (svalue,'a) token
val DARROW: (string) *  'a * 'a -> (svalue,'a) token
val ARROW: (string) *  'a * 'a -> (svalue,'a) token
val DOT: (string) *  'a * 'a -> (svalue,'a) token
val COMMA: (string) *  'a * 'a -> (svalue,'a) token
val SEMICOLON: (string) *  'a * 'a -> (svalue,'a) token
val TIMES: (string) *  'a * 'a -> (svalue,'a) token
val EQUALOP: (string) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val CHAR: (string) *  'a * 'a -> (svalue,'a) token
val WORD: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (string) *  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
val EOF: ( ( int list * int list * int list )  ref) *  'a * 'a -> (svalue,'a) token
end
signature AssertEngine_LRVALS=
sig
structure Tokens : AssertEngine_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
