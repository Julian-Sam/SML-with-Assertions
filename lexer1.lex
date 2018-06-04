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
%header (functor SampleLexFun(structure Tokens: Sample_TOKENS));
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
\n           => (pos := (!pos) + 1; 
				 Tokens.NEWLINE (yytext, !pos, !pos));
{ws}+        => (lex());
{integer}    => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));

"*"          => (Tokens.TIMES(yytext, !pos,!pos));
"/"          => (Tokens.REALDIV(yytext, !pos,!pos));
"div"	     => (Tokens.INTDIV(yytext, !pos,!pos));
"+"          => (Tokens.PLUS(yytext, !pos,!pos));
"-"          => (Tokens.SUB(yytext, !pos,!pos));
"^"          => (Tokens.CARAT(yytext, !pos,!pos));
";"			 => (Tokens.SEMICOLON (yytext, !pos, !pos));
{id}		 => (Tokens.ID(yytext, !pos, !pos));

"."          => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


