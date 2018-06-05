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
\n           => (pos := (!pos) + 1; lex());
{ws}+        => (lex());
{integer}    => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));
{hex}        => (Tokens.HEX(yytext, !pos,!pos));
{word}       => (Tokens.WORD(yytext, !pos,!pos));

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
"+"          => (Tokens.PLUS(yytext, !pos,!pos));
"-"          => (Tokens.SUB(yytext, !pos,!pos));
"^"          => (Tokens.CARAT(yytext, !pos,!pos));

"|"		     => (Tokens.BAR(yytext, !pos,!pos));
"!"		     => (Tokens.BANG(yytext, !pos,!pos));
":"		     => (Tokens.COLON(yytext, !pos,!pos));
":>"	     => (Tokens.COLONGT(yytext, !pos,!pos));
"#"	 	     => (Tokens.HASH(yytext, !pos,!pos));
"->"	     => (Tokens.ARROW(yytext, !pos,!pos));
"=>"	     => (Tokens.DARROW(yytext, !pos,!pos));
";"          => (Tokens.SEMICOLON(yytext, !pos,!pos));
"o"          => (Tokens.COMPOSITION(yytext, !pos,!pos));

"(*"	     => (Tokens.LCOMMENTPAREN(yytext, !pos,!pos));
"*)"	     => (Tokens.RCOMMENTPAREN(yytext, !pos,!pos));

"("	 	     => (Tokens.LPAREN(yytext, !pos,!pos));
")"	 	     => (Tokens.RPAREN(yytext, !pos,!pos));

"#["	     => (Tokens.VECTORSTART(yytext, !pos,!pos));
"["	 	     => (Tokens.LBRACK(yytext, !pos,!pos));
"]"	 	     => (Tokens.RBRACK(yytext, !pos,!pos));

"(!"	 	 => (Tokens.LASSERT(yytext, !pos,!pos));
"!)"	 	 => (Tokens.RASSERT(yytext, !pos,!pos));

"requires"   => (Tokens.REQUIRES(yytext, !pos, !pos));
"ensures"    => (Tokens.ENSURES(yytext, !pos, !pos));

"true"	     => (Tokens.TRUE(yytext, !pos,!pos));
"false"	     => (Tokens.FALSE(yytext, !pos,!pos));

"not"	     => (Tokens.NOT(yytext, !pos,!pos));
"or"	     => (Tokens.OR(yytext, !pos,!pos));
"and"	     => (Tokens.AND(yytext, !pos,!pos));
"orelse"     => (Tokens.ORELSE(yytext, !pos,!pos));
"andalso"    => (Tokens.ANDALSO(yytext, !pos,!pos));

"if"	     => (Tokens.IF(yytext, !pos,!pos));
"then"	     => (Tokens.THEN(yytext, !pos,!pos));
"else"	     => (Tokens.ELSE(yytext, !pos,!pos));

"let"	     => (Tokens.LET(yytext, !pos,!pos));
"in"	     => (Tokens.IN(yytext, !pos,!pos));
"end"	     => (Tokens.END(yytext, !pos,!pos));

"val"	     => (Tokens.VAL(yytext, !pos,!pos));

"fun"	     => (Tokens.FUN(yytext, !pos,!pos));
"fn"	     => (Tokens.FN(yytext, !pos,!pos));

"functor"    => (Tokens.FUNCTOR(yytext, !pos, !pos));
"funsig"     => (Tokens.FUNSIG(yytext, !pos, !pos));

"handle"     => (Tokens.HANDLE(yytext, !pos,!pos));
"raise"	     => (Tokens.RAISE(yytext, !pos,!pos));
"exception"	 => (Tokens.EXCEPTION(yytext, !pos,!pos));

"case"	     => (Tokens.CASE(yytext, !pos,!pos));
"of"	     => (Tokens.OF(yytext, !pos,!pos));

"type"	     => (Tokens.TYPE(yytext, !pos,!pos));
"datatype"	 => (Tokens.DATATYPE(yytext, !pos,!pos));

"ref"	     => (Tokens.REF(yytext, !pos,!pos));

"SOME"	     => (Tokens.SOME(yytext, !pos,!pos));
"NONE"	     => (Tokens.NONE(yytext, !pos,!pos));

"as"	     => (Tokens.AS(yytext, !pos,!pos));
"abstype"	 => (Tokens.ABSTYPE(yytext, !pos,!pos));

"open"	     => (Tokens.OPEN(yytext, !pos,!pos));
"with"	     => (Tokens.WITH(yytext, !pos,!pos));
"withtype"	 => (Tokens.WITHTYPE(yytext, !pos,!pos));

"while"	     => (Tokens.WHILE(yytext, !pos,!pos));
"do"	     => (Tokens.DO(yytext, !pos,!pos));

"where"	     => (Tokens.WHERE(yytext, !pos,!pos));

"sig"	     => (Tokens.SIG(yytext, !pos, !pos));
"struct"     => (Tokens.STRUCT(yytext, !pos, !pos));
"signature"	 => (Tokens.SIGNATURE(yytext, !pos, !pos));
"structure"	 => (Tokens.STRUCTURE(yytext, !pos, !pos));

{id}		 => (Tokens.ID(yytext, !pos, !pos));

"."          => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


