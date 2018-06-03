structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = print ( String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor SMLLexFun(structure Tokens: Sample_TOKENS));
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexnum=[0-9a-fA-F]+;
integer = (~?){num};
hex = (~?)"0x"{hexnum};
word = "0w"{num}|"0wx"{hexnum};

%%

(* todo: change newline and ws to tokens for stringify *)

\n           => (pos := (!pos) + 1; lex());
{ws}+        => (lex());
{integer}    => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));
{hex}        => (Tokens.HEX(!pos,!pos));
{word}       => (Tokens.WORD(!pos,!pos));

(* SYMBOLS *)

	(* Comparision and Assignment*)
"<>"	     => (Tokens.UNEQUAL(!pos,!pos));
"="	         => (Tokens.EQUAL(!pos,!pos));
":="	     => (Tokens.VARASSIGN(!pos,!pos));
">"		     => (Tokens.GREATERTHAN(!pos,!pos));
"<"		     => (Tokens.LESSTHAN(!pos,!pos));
">="	     => (Tokens.GREATERTHANEQUAL(!pos,!pos));
"<="	     => (Tokens.LESSTHAN(!pos,!pos));

	(* Arithmetic *)
"*"          => (Tokens.TIMES(!pos,!pos));
"/"          => (Tokens.REALDIV(!pos,!pos));
"div"	     => Tokens.INTDIV(!pos,!pos);
"+"          => (Tokens.PLUS(!pos,!pos));
"-"          => (Tokens.SUB(!pos,!pos));
"^"          => (Tokens.CARAT(!pos,!pos));

	(* Misc *)
"|"		     => (Tokens.BAR(!pos,!pos));
"!"		     => (Tokens.BANG(!pos,!pos));
":"		     => (Tokens.COLON(!pos,!pos));
":>"	     => (Tokens.COLONGT(!pos,!pos));
"="	 	     => (Tokens.EQUALOP(!pos,!pos));
"#"	 	     => (Tokens.HASH(!pos,!pos));
"->"	     => (Tokens.ARROW(!pos,!pos));
"=>"	     => (Tokens.DARROW(!pos,!pos));
";"          => (Tokens.SEMICOLON(!pos,!pos));
"o"          => (Tokens.COMPOSITION(!pos,!pos));

(* Comments and Parens *)
"(*"	     => (Tokens.LCOMMENTPAREN(!pos,!pos));
"*)"	     => (Tokens.RCOMMENTPAREN(!pos,!pos));

"("	 	     => (Tokens.LPAREN(!pos,!pos));
")"	 	     => (Tokens.RPAREN(!pos,!pos));

"#["	     => (Tokens.VECTORSTART(!pos,!pos));
"["	 	     => (Tokens.LBRACK(!pos,!pos));
"]"	 	     => (Tokens.RBRACK(!pos,!pos));


(* Identifiers *)
"true"	     => (Tokens.TRUE(!pos,!pos));
"false"	     => (Tokens.FALSE(!pos,!pos));

"not"	     => (Tokens.NOT(!pos,!pos));
"or"	     => (Tokens.OR(!pos,!pos));
"and"	     => (Tokens.AND(!pos,!pos));
"orelse"     => (Tokens.ORELSE(!pos,!pos));
"andalso"    => (Tokens.ANDALSO(!pos,!pos));

"if"	     => (Tokens.IF(!pos,!pos));
"then"	     => (Tokens.THEN(!pos,!pos));
"else"	     => (Tokens.ELSE(!pos,!pos));

"let"	     => (Tokens.LET(!pos,!pos));
"in"	     => (Tokens.IN(!pos,!pos));
"end"	     => (Tokens.END(!pos,!pos));

"val"	     => (Tokens.VAL(!pos,!pos));

"fun"	     => (Tokens.FUN(!pos,!pos));
"fn"	     => (Tokens.FN(!pos,!pos));

"functor"    => (Tokens.FUNCTOR(!pos, !pos));
"funsig"     => (Tokens.FUNSIG(!pos, !pos));

"handle"     => (Tokens.HANDLE(!pos,!pos));
"raise"	     => (Tokens.RAISE(!pos,!pos));
"exception"	 => (Tokens.EXCEPTION(!pos,!pos));

"case"	     => (Tokens.CASE(!pos,!pos));
"of"	     => (Tokens.OF(!pos,!pos));

"type"	     => (Tokens.TYPE(!pos,!pos));
"datatype"	 => (Tokens.DATATYPE(!pos,!pos));

"ref"	     => (Tokens.REF(!pos,!pos));

"SOME"	     => (Tokens.SOME(!pos,!pos));
"NONE"	     => (Tokens.NONE(!pos,!pos));

"as"	     => (Tokens.AS(!pos,!pos));
"abstype"	 => (Tokens.ABSTYPE(!pos,!pos));
"exception"	 => (Tokens.EXCEPTION(!pos,!pos));

"open"	     => (Tokens.OPEN(!pos,!pos));
"with"	     => (Tokens.WITH(!pos,!pos));
"withtype"	 => (Tokens.WITHTYPE(!pos,!pos));

"while"	     => (Tokens.WHILE(!pos,!pos));
"do"	     => (Tokens.DO(!pos,!pos));

"where"	     => (Tokens.WHERE(!pos,!pos));

"sig"	     => (Tokens.SIG(!pos, !pos));
"struct"     => (Tokens.STRUCT(!pos, !pos));
"signature"	 => (Tokens.SIGNATURE(!pos, !pos));
"structure"	 => (Tokens.STRUCTURE(!pos, !pos));


"."          => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


