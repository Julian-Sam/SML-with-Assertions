structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 1
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = print ( String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor SampleLexFun(structure Tokens: Sample_TOKENS));
alpha = [A-Za-z];
idchars=[A-Za-z'_0-9];
id={alpha}{idchars}*;
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
notquote = [^"];
chars = [.];
char = ("#\""){chars}("\"");
string = ("\""){notquote}("\"");

%%
\n           => (pos := (!pos) + 1; lex ());
{ws}+        => (lex());
{integer}    => (Tokens.INT  (yytext, !pos, !pos));
{real}		 => (Tokens.REAL (yytext, !pos, !pos));
{char}		 => (Tokens.CHAR (yytext, !pos, !pos));
{string}	 => (Tokens.STR (yytext, !pos, !pos));

"*"          => (Tokens.TIMES(!pos,!pos));
"/"          => (Tokens.REALDIV(!pos,!pos));
"div"	     => (Tokens.INTDIV(!pos,!pos));
"+"          => (Tokens.PLUS(!pos,!pos));
"-"          => (Tokens.SUB(!pos,!pos));
";"			 => (Tokens.SEMICOLON (!pos, !pos));

"<>"	     => (Tokens.UNEQUALOP(!pos,!pos));
"="	         => (Tokens.EQUALOP(!pos,!pos));
">"		     => (Tokens.GREATERTHAN(!pos,!pos));
"<"		     => (Tokens.LESSTHAN(!pos,!pos));
">="	     => (Tokens.GREATERTHANEQ(!pos,!pos));
"<="	     => (Tokens.LESSTHANEQ(!pos,!pos));
":="	     => (Tokens.VARASSIGN(!pos,!pos));

"["	 	     => (Tokens.LBRACK(!pos,!pos));
"]"	 	     => (Tokens.RBRACK(!pos,!pos));

"if"	     => (Tokens.IF(!pos,!pos));
"then"	     => (Tokens.THEN(!pos,!pos));
"else"	     => (Tokens.ELSE(!pos,!pos));

"let"	     => (Tokens.LET(!pos,!pos));
"in"	     => (Tokens.IN(!pos,!pos));
"end"	     => (Tokens.END(!pos,!pos));

"true"	     => (Tokens.TRUE(!pos,!pos));
"false"	     => (Tokens.FALSE(!pos,!pos));

"val"	     => (Tokens.VAL(!pos,!pos));
"fun"	     => (Tokens.FUN(!pos,!pos));
"fn"	     => (Tokens.FN(!pos,!pos));
"handle"     => (Tokens.HANDLE(!pos,!pos));
"raise"	     => (Tokens.RAISE(!pos,!pos));
"case"	     => (Tokens.CASE(!pos,!pos));
"of"	     => (Tokens.OF(!pos,!pos));
"as"	     => (Tokens.AS(!pos,!pos));
"while"	     => (Tokens.WHILE(!pos,!pos));
"do"	     => (Tokens.DO(!pos,!pos));

"("	 	     => (Tokens.LPAREN(!pos,!pos));
")"	 	     => (Tokens.RPAREN(!pos,!pos));

"|"		     => (Tokens.BAR(!pos,!pos));
"!"		     => (Tokens.BANG(!pos,!pos));
"=>"	     => (Tokens.DARROW(!pos,!pos));
";"          => (Tokens.SEMICOLON(!pos,!pos));
","			 => (Tokens.COMMA(!pos, !pos));
"_"			 => (Tokens.WILDCARD(!pos, !pos));

{id}		 => (Tokens.ID(yytext, !pos, !pos));


"."          => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


