signature Sample_TOKENS =
sig
type ('a,'b) token
type svalue
val SEMICOLON: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val CARAT: (string) *  'a * 'a -> (svalue,'a) token
val REALDIV: (string) *  'a * 'a -> (svalue,'a) token
val SUB: (string) *  'a * 'a -> (svalue,'a) token
val INTDIV: (string) *  'a * 'a -> (svalue,'a) token
val TIMES: (string) *  'a * 'a -> (svalue,'a) token
val PLUS: (string) *  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature Sample_LRVALS=
sig
structure Tokens : Sample_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
