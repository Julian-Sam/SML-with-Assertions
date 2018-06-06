signature Sample_TOKENS =
sig
type ('a,'b) token
type svalue
val ID_NAME: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (string) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val CHAR: (string) *  'a * 'a -> (svalue,'a) token
val WORD: (string) *  'a * 'a -> (svalue,'a) token
val EXCEPTION: (string) *  'a * 'a -> (svalue,'a) token
val RAISE: (string) *  'a * 'a -> (svalue,'a) token
val HANDLE: (string) *  'a * 'a -> (svalue,'a) token
val ANDALSO: (string) *  'a * 'a -> (svalue,'a) token
val ORELSE: (string) *  'a * 'a -> (svalue,'a) token
val ELSE: (string) *  'a * 'a -> (svalue,'a) token
val THEN: (string) *  'a * 'a -> (svalue,'a) token
val IF: (string) *  'a * 'a -> (svalue,'a) token
val DO: (string) *  'a * 'a -> (svalue,'a) token
val WHILE: (string) *  'a * 'a -> (svalue,'a) token
val END: (string) *  'a * 'a -> (svalue,'a) token
val IN: (string) *  'a * 'a -> (svalue,'a) token
val LET: (string) *  'a * 'a -> (svalue,'a) token
val LESSTHANEQUAL: (string) *  'a * 'a -> (svalue,'a) token
val GREATERTHANEQUAL: (string) *  'a * 'a -> (svalue,'a) token
val LESSTHAN: (string) *  'a * 'a -> (svalue,'a) token
val GREATERTHAN: (string) *  'a * 'a -> (svalue,'a) token
val VARASSIGN: (string) *  'a * 'a -> (svalue,'a) token
val EQUALOP: (string) *  'a * 'a -> (svalue,'a) token
val UNEQUAL: (string) *  'a * 'a -> (svalue,'a) token
val COMPOSITION: (string) *  'a * 'a -> (svalue,'a) token
val COLONGT: (string) *  'a * 'a -> (svalue,'a) token
val BANG: (string) *  'a * 'a -> (svalue,'a) token
val COLON: (string) *  'a * 'a -> (svalue,'a) token
val BAR: (string) *  'a * 'a -> (svalue,'a) token
val DARROW: (string) *  'a * 'a -> (svalue,'a) token
val ARROW: (string) *  'a * 'a -> (svalue,'a) token
val RBRACK: (string) *  'a * 'a -> (svalue,'a) token
val LBRACK: (string) *  'a * 'a -> (svalue,'a) token
val SEMICOLON: (string) *  'a * 'a -> (svalue,'a) token
val CARAT: (string) *  'a * 'a -> (svalue,'a) token
val REALDIV: (string) *  'a * 'a -> (svalue,'a) token
val RPAREN: (string) *  'a * 'a -> (svalue,'a) token
val LPAREN: (string) *  'a * 'a -> (svalue,'a) token
val SUB: (string) *  'a * 'a -> (svalue,'a) token
val INTDIV: (string) *  'a * 'a -> (svalue,'a) token
val TIMES: (string) *  'a * 'a -> (svalue,'a) token
val PLUS: (string) *  'a * 'a -> (svalue,'a) token
val COMMA: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
end
signature Sample_LRVALS=
sig
structure Tokens : Sample_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
