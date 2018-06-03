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
%%
(* todo: change newline and ws to tokens for stringify *)
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{num}+   => (Tokens.INT (valOf (Int.fromString yytext), !pos, !pos));

(* SYMBOLS *)
">"		 => Tokens.GREATERTHAN
"<"		 => Tokens.LESSTHAN
">="	 => Tokens.GREATERTHANEQUAL
"<="	 => Tokens.LESSTHAN

"<>"	 => Tokens.UNEQUAL
"="	     => Tokens.EQUAL
":="	 => Tokens.VARASSIGN

"*"      => (Tokens.TIMES(!pos,!pos));
"/"      => (Tokens.REALDIV(!pos,!pos));
"div"	 => Tokens.INTDIV(!pos,!pos);
"+"      => (Tokens.PLUS(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"^"      => (Tokens.CARAT(!pos,!pos));

"|"		 => Tokens.BAR(!pos,!pos);
":"		 => Tokens.COLON(!pos,!pos);
":>"	 => Tokens.COLONGT(!pos,!pos);
"="	 	 => Tokens.EQUALOP(!pos,!pos);
"#"	 	 => Tokens.HASH(!pos,!pos);
"->"	 => Tokens.ARROW(!pos,!pos);
"=>"	 => Tokens.DARROW(!pos,!pos);
";"      => (Tokens.SEMI(!pos,!pos));

"o"      => (Tokens.COMPOSITION(!pos,!pos));

(* Comments and Parens *)
"(*"	 => Tokens.LCOMMENTPAREN(!pos,!pos);
"*)"	 => Tokens.RCOMMENTPAREN(!pos,!pos);
"("	 	 => Tokens.LPAREN(!pos,!pos);
")"	 	 => Tokens.RPAREN(!pos,!pos);



(* Identifiers *)
"if"	 => (Tokens.IF(!pos,!pos));
"then"	 => (Tokens.THEN(!pos,!pos));
"else"	 => (Tokens.ELSE(!pos,!pos));

"let"	 => (Tokens.LET(!pos,!pos));
"in"	 => (Tokens.IN(!pos,!pos));
"end"	 => (Tokens.END(!pos,!pos));

"fun"	 => (Tokens.FUN(!pos,!pos));
"fn"	 => (Tokens.FN(!pos,!pos));

"handle" => (Tokens.HANDLE(!pos,!pos));
"raise"	 => (Tokens.RAISE(!pos,!pos));

"case"	 => (Tokens.CASE(!pos,!pos));
"of"	 => (Tokens.OF(!pos,!pos));



{alpha}+ => (if yytext="print"
                 then Tokens.PRINT(!pos,!pos)
                 else Tokens.ID(yytext,!pos,!pos)
            );

"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


