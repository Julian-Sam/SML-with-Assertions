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
ascii = [^\n] | \n;
num=[0-9]+;
frac="."{num};
exp=[eE](~?){num};
real_=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
hexnum=[0-9a-fA-F]+;
integer = (~?){num};
hex = (~?)"0x"{hexnum};
word_ = "0w"{num}|"0wx"{hexnum};
char_ = "#\""{ascii}"\"";
string_ = "\""{ascii}*"\"";

%%
"\n"         => (pos := (!pos) + 1; lex());
{ws}+        => (lex());
{integer}    => (Tokens.INT(yytext, !pos, !pos));
{hex}        => (Tokens.INT(yytext, !pos, !pos));
{real_}		 => (Tokens.REAL(yytext, !pos, !pos));
{word_}      => (Tokens.WORD(yytext, !pos,!pos));
{char_}		 => (Tokens.CHAR(yytext, !pos, !pos));
{string_}	 => (Tokens.STRING(yytext, !pos, !pos));

"'"{idchars}+

"<>"	     => (Tokens.UNEQUAL(yytext, !pos,!pos));
"="	         => (Tokens.EQUALOP(yytext, !pos,!pos));
":="	     => (Tokens.VARASSIGN(yytext, !pos,!pos));
">"		     => (Tokens.GREATERTHAN(yytext, !pos,!pos));
"<"		     => (Tokens.LESSTHAN(yytext, !pos,!pos));
">="	     => (Tokens.GREATERTHANEQUAL(yytext, !pos,!pos));
"<="	     => (Tokens.LESSTHANEQUAL(yytext, !pos,!pos));

"*"          => (Tokens.TIMES(yytext, !pos,!pos));
"/"          => (Tokens.REALDIV(yytext, !pos,!pos));
"div"	     => (Tokens.INTDIV(yytext, !pos,!pos));
"mod"		 => (Tokens.MOD(yytext, !pos, !pos));
"+"          => (Tokens.PLUS(yytext, !pos,!pos));
"-"          => (Tokens.SUB(yytext, !pos,!pos));
"^"          => (Tokens.CARAT(yytext, !pos,!pos));
";"			 => (Tokens.SEMICOLON (yytext, !pos, !pos));
","          => (Tokens.COMMA(yytext, !pos,!pos));
"."			 => (Tokens.DOT (yytext, !pos, !pos));


"->"	     => (Tokens.ARROW(yytext, !pos,!pos));
"=>"	     => (Tokens.DARROW(yytext, !pos,!pos));
"|"		     => (Tokens.BAR(yytext, !pos,!pos));
":"		     => (Tokens.COLON(yytext, !pos,!pos));
"::"		 => (Tokens.DCOLON(yytext, !pos,!pos));
"!"		     => (Tokens.BANG(yytext, !pos,!pos));
":>"	     => (Tokens.COLONGT(yytext, !pos,!pos));
"o"          => (Tokens.COMPOSITION(yytext, !pos,!pos));
"#"          => (Tokens.HASH(yytext, !pos,!pos));

"{"	 	     => (Tokens.LCURLY(yytext, !pos,!pos));
"}"	 	     => (Tokens.RCURLY(yytext, !pos,!pos));


"["	 	     => (Tokens.LBRACK(yytext, !pos,!pos));
"]"	 	     => (Tokens.RBRACK(yytext, !pos,!pos));
"("	 	     => (Tokens.LPAREN(yytext, !pos,!pos));
")"	 	     => (Tokens.RPAREN(yytext, !pos,!pos));

"nil"	 	 => (Tokens.NIL(yytext, !pos,!pos));

"if"	     => (Tokens.IF(yytext, !pos,!pos));
"then"	     => (Tokens.THEN(yytext, !pos,!pos));
"else"	     => (Tokens.ELSE(yytext, !pos,!pos));

"while"	     => (Tokens.WHILE(yytext, !pos,!pos));
"do"	     => (Tokens.DO(yytext, !pos,!pos));

"let"	     => (Tokens.LET(yytext, !pos,!pos));
"in"	     => (Tokens.IN(yytext, !pos,!pos));
"end"	     => (Tokens.END(yytext, !pos,!pos));

"orelse"     => (Tokens.ORELSE(yytext, !pos,!pos));
"andalso"    => (Tokens.ANDALSO(yytext, !pos,!pos));

"handle"     => (Tokens.HANDLE(yytext, !pos,!pos));
"raise"	     => (Tokens.RAISE(yytext, !pos,!pos));
"exception"	 => (Tokens.EXCEPTION(yytext, !pos,!pos));

"val"		 => (Tokens.VAL(yytext, !pos, !pos));
"and"		 => (Tokens.AND(yytext, !pos, !pos));
"fn" 		 => (Tokens.FN(yytext, !pos, !pos));
"fun"		 => (Tokens.FUN(yytext, !pos, !pos));
"case" 		 => (Tokens.CASE(yytext, !pos, !pos));
"of"	     => (Tokens.OF(yytext, !pos, !pos));
"_"			 => (Tokens.WILD(yytext, !pos, !pos));

{id}		 => (Tokens.ID_NAME(yytext, !pos, !pos));

"."          => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());

