
structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

exception UnmatchedComments
val pos = ref 0
val unmatched_comments = ref 0
fun inc(x) = x := (!x + 1)
fun dec(x) = x := (!x - 1)
fun eof () = (if (!unmatched_comments) <> 0 
			 then (print("Error: Unmatched Comment Bracket"); raise UnmatchedComments)
			 else Tokens.EOF(!pos,!pos))
fun error (e,l : int,_) = print ( String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor SampleLexFun(structure Tokens: Sample_TOKENS));
%reject;
%s COMMENT;
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
nrws=("\012"|[\t\ ])+;
ascii = [^\n] | \n;
num=[0-9]+;
some_sym=[!%&$+/:<=>?@~|#*]|\-|\^;
sym={some_sym}|"\\";
quote="`";
symbol={sym}|{quote};
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
<INITIAL>"\n"        => (pos := (!pos) + 1; lex());
<INITIAL>{ws}+       => (lex());
<INITIAL>{integer}   => (Tokens.INT(yytext, !pos, !pos));
<INITIAL>{hex}       => (Tokens.INT(yytext, !pos, !pos));
<INITIAL>{real_}	 => (Tokens.REAL(yytext, !pos, !pos));
<INITIAL>{word_}     => (Tokens.WORD(yytext, !pos,!pos));
<INITIAL>{char_}	 => (Tokens.CHAR(yytext, !pos, !pos));
<INITIAL>{string_}	 => (Tokens.STRING(yytext, !pos, !pos));

<INITIAL>"="	     => (Tokens.EQUALOP(yytext, !pos,!pos));
<INITIAL>"*"         => (Tokens.TIMES(yytext, !pos,!pos));

<INITIAL>";"		 => (Tokens.SEMICOLON (yytext, !pos, !pos));
<INITIAL>","         => (Tokens.COMMA(yytext, !pos,!pos));
<INITIAL>"."		 => (Tokens.DOT (yytext, !pos, !pos));

<INITIAL>"->"	     => (Tokens.ARROW(yytext, !pos,!pos));
<INITIAL>"=>"	     => (Tokens.DARROW(yytext, !pos,!pos));
<INITIAL>"|"		 => (Tokens.BAR(yytext, !pos,!pos));
<INITIAL>":"		 => (Tokens.COLON(yytext, !pos,!pos));
<INITIAL>"::"		 => (Tokens.DCOLON(yytext, !pos,!pos));
<INITIAL>"!"		 => (Tokens.BANG(yytext, !pos,!pos));
<INITIAL>":>"	     => (Tokens.COLONGT(yytext, !pos,!pos));
<INITIAL>"#"         => (Tokens.HASH(yytext, !pos,!pos));

<INITIAL>"{"	 	 => (Tokens.LCURLY(yytext, !pos,!pos));
<INITIAL>"}"	 	 => (Tokens.RCURLY(yytext, !pos,!pos));

<INITIAL>"(*"		 => (YYBEGIN COMMENT; unmatched_comments := 1; lex());
<INITIAL>"*)"		 => (error("Error: unmatched close comment", !pos, !pos); lex());

<COMMENT>"(*"		 => (inc unmatched_comments; lex());
<COMMENT>"*)"		 => (dec unmatched_comments; 
						 if (!unmatched_comments) = 0
						 then YYBEGIN INITIAL else (); 
						 lex());

<COMMENT>\n  		 => (lex());
<COMMENT>.  		 => (lex());

<INITIAL>"["	 	 => (Tokens.LBRACK(yytext, !pos,!pos));
<INITIAL>"]"	 	 => (Tokens.RBRACK(yytext, !pos,!pos));
<INITIAL>"("	 	 => (Tokens.LPAREN(yytext, !pos,!pos));
<INITIAL>")"	 	 => (Tokens.RPAREN(yytext, !pos,!pos));

<INITIAL>"if"	     => (Tokens.IF(yytext, !pos,!pos));
<INITIAL>"then"	     => (Tokens.THEN(yytext, !pos,!pos));
<INITIAL>"else"	     => (Tokens.ELSE(yytext, !pos,!pos));

<INITIAL>"while"	 => (Tokens.WHILE(yytext, !pos,!pos));
<INITIAL>"do"	     => (Tokens.DO(yytext, !pos,!pos));

<INITIAL>"let"	     => (Tokens.LET(yytext, !pos,!pos));
<INITIAL>"in"	     => (Tokens.IN(yytext, !pos,!pos));
<INITIAL>"end"	     => (Tokens.END(yytext, !pos,!pos));

<INITIAL>"orelse"    => (Tokens.ORELSE(yytext, !pos,!pos));
<INITIAL>"andalso"   => (Tokens.ANDALSO(yytext, !pos,!pos));

<INITIAL>"handle"    => (Tokens.HANDLE(yytext, !pos,!pos));
<INITIAL>"raise"	 => (Tokens.RAISE(yytext, !pos,!pos));
<INITIAL>"exception" => (Tokens.EXCEPTION(yytext, !pos,!pos));

<INITIAL>"val"		 => (Tokens.VAL(yytext, !pos, !pos));
<INITIAL>"and"		 => (Tokens.AND(yytext, !pos, !pos));
<INITIAL>"fn" 		 => (Tokens.FN(yytext, !pos, !pos));
<INITIAL>"fun"		 => (Tokens.FUN(yytext, !pos, !pos));
<INITIAL>"case" 	 => (Tokens.CASE(yytext, !pos, !pos));
<INITIAL>"of"	     => (Tokens.OF(yytext, !pos, !pos));
<INITIAL>"_"		 => (Tokens.WILD(yytext, !pos, !pos));
<INITIAL>"op"		 => (Tokens.OP(yytext, !pos, !pos));
<INITIAL>"rec"		 => (Tokens.REC(yytext, !pos, !pos));

<INITIAL>"type" 	 => (Tokens.TYPE(yytext, !pos, !pos));
<INITIAL>"datatype"  => (Tokens.DATATYPE(yytext, !pos, !pos));
<INITIAL>"abstype"	 => (Tokens.ABSTYPE(yytext, !pos, !pos));
<INITIAL>"with"	 	 => (Tokens.WITH(yytext, !pos, !pos));
<INITIAL>"withtype"	 => (Tokens.WITHTYPE(yytext, !pos, !pos));
<INITIAL>"as"		 => (Tokens.AS(yytext, !pos, !pos));
<INITIAL>"open"		 => (Tokens.OPEN(yytext, !pos, !pos));
<INITIAL>"local"	 => (Tokens.LOCAL(yytext, !pos, !pos));
<INITIAL>"infix" 	 => (Tokens.INFIX(yytext, !pos, !pos));
<INITIAL>"infixr"	 => (Tokens.INFIXR(yytext, !pos, !pos));
<INITIAL>"nonfix"	 => (Tokens.NONFIX(yytext, !pos, !pos));

<INITIAL>"struct"	 => (Tokens.STRUCT(yytext, !pos, !pos));
<INITIAL>"structure" => (Tokens.STRUCTURE(yytext, !pos, !pos));
<INITIAL>"sig" 		 => (Tokens.SIG(yytext, !pos, !pos));
<INITIAL>"signature" => (Tokens.SIGNATURE(yytext, !pos, !pos));
<INITIAL>"functor"   => (Tokens.FUNCTOR(yytext, !pos, !pos));

<INITIAL>"include" 	 => (Tokens.INCLUDE(yytext, !pos, !pos));
<INITIAL>"where" 	 => (Tokens.WHERE(yytext, !pos, !pos));
<INITIAL>"eqtype" 	 => (Tokens.EQTYPE(yytext, !pos, !pos));
<INITIAL>"sharing" 	 => (Tokens.SHARING(yytext, !pos, !pos));

<INITIAL>"..."		 => (Tokens.DOTTED_WILDCARD(yytext, !pos, !pos));

<INITIAL>"(*!"       => (Tokens.LASSERT(yytext, !pos, !pos));
<INITIAL>"!*)"       => (Tokens.RASSERT(yytext, !pos, !pos));
<INITIAL>"REQUIRES"  => (Tokens.REQUIRES(yytext, !pos, !pos));
<INITIAL>"ENSURES"   => (Tokens.ENSURES(yytext, !pos, !pos));

<INITIAL>{symbol}+   => (if yytext = ":" orelse
							yytext = "|" orelse
							yytext = "=" orelse
							yytext = "#" then REJECT()
						else Tokens.SYMBOLS(yytext, !pos, !pos));

<INITIAL>"'"{idchars}+    => (Tokens.QUOTE_ID(yytext, !pos, !pos));

<INITIAL>{id}		 => (Tokens.ID(yytext, !pos, !pos));
<INITIAL>.           => (error ("ignoring bad character "^yytext,!pos,!pos); lex());

