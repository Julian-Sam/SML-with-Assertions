structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
exception UnmatchedComments
val pos = ref 1
fun printf x = print x;
datatype ws_type = WS of int | NL of int | TAB of int | Comment of string * int


val ws_type_list_ref: (int list * int list * int list) ref = ref ([], [], [])
val final_ref_list: (int list * int list * int list) ref = ref ([], [], [])
val string_ref =  ref ("", 0)

fun req num = let
			   val (ws_list, nl_list, _) = !ws_type_list_ref
			 in
			   ws_type_list_ref := (num :: ws_list, nl_list, [])
			 end

fun ens num = let
			   val (ws_list, nl_list, _) = !ws_type_list_ref
			 in
			   ws_type_list_ref := (ws_list, num :: nl_list, [])
			 end

fun rev_str (str: string) = String.implode (List.rev (String.explode (str)))

fun printf_Ints (x) =
  case x of 
    nil => printf ("[]\n\n")
  | i :: x' => (let
                   val _ = printf (Int.toString (i) ^ " :: ")
                 in
                   printf_Ints (x')
                 end)

val unmatched_comments = ref 0
fun inc(x) = x := (!x + 1)
fun dec(x) = x := (!x - 1)

fun eof () = (if (!unmatched_comments) <> 0 
			 then (printf("Error: Unmatched Comment Bracket"); raise UnmatchedComments)
			 else (let
					 val (ws, nl, _) = !ws_type_list_ref
					 val final_ref_list = ref (List.rev (ws), List.rev (nl), [])
				   in
					 pos := 1;
					 ws_type_list_ref := ([], [], []);
					 Tokens.EOF(final_ref_list, !pos, !pos)
				   end))

fun error (e,l : int,_) = printf (String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor SampleLexFun(structure Tokens: Sample_TOKENS));
%reject
%s COMMENT; 
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws=("\012"|[\t\ ])*;
space = " ";
tab = [\t];
newline = [\n];
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
ordinal_alphabet = "\\"[0-9]{3};
ordinal_hex = "\\u"[0-9a-fA-F]{4};
control_chars = "\\^"{ascii};
escape_chars = "\\"[abtnvfr];
chars = {ascii} | {escape_chars} | {control_chars} | {ordinal_hex} | {ordinal_alphabet};
char_ = "#\""{chars}"\"";
two_backslash = "\\\\";
even_backslashes = {two_backslash}*;
everything_but_quotes = [^\"];
everything_but_backslash = [^\\];
empty_string = "\"\"";
non_empty_string = "\""{everything_but_quotes}*{even_backslashes}"\"";
string_ = {non_empty_string};
%%
<INITIAL>{space}     => (printf "space\n"; lex());
<INITIAL>{tab}       => (printf "tab\n"; lex());
<INITIAL>{newline}   => (printf "newline\n"; pos := !pos + 1; lex());

<INITIAL>{integer}   => (printf "int\n"; Tokens.INT(yytext, !pos, !pos));
<INITIAL>{hex}       => (printf "1\n"; Tokens.INT(yytext, !pos, !pos));
<INITIAL>{real_}	 => (printf "1\n"; Tokens.REAL(yytext, !pos, !pos));
<INITIAL>{word_}     => (printf "1\n"; Tokens.WORD(yytext, !pos,!pos));
<INITIAL>{char_}	 => (printf "1\n"; Tokens.CHAR(yytext, !pos, !pos));

<INITIAL>{string_}	 => (printf "string\n"; Tokens.STRING(yytext, !pos, !pos));

<INITIAL>"="	     => (printf "equalop\n"; Tokens.EQUALOP(yytext, !pos,!pos));
<INITIAL>"*"         => (printf "1\n"; Tokens.TIMES(yytext, !pos,!pos));

<INITIAL>";"		 => (printf "semicolon\n"; Tokens.SEMICOLON (yytext, !pos, !pos));
<INITIAL>","         => (printf "1\n"; Tokens.COMMA(yytext, !pos,!pos));
<INITIAL>"."		 => (printf "1\n"; Tokens.DOT (yytext, !pos, !pos));

<INITIAL>"->"	     => (printf "2\n"; Tokens.ARROW(yytext, !pos,!pos));
<INITIAL>"=>"	     => (printf "2\n"; Tokens.DARROW(yytext, !pos,!pos));
<INITIAL>"|"		 => (printf "2\n"; Tokens.BAR(yytext, !pos,!pos));
<INITIAL>":"		 => (printf "2\n"; Tokens.COLON(yytext, !pos,!pos));
<INITIAL>":>"	     => (printf ":>\n"; Tokens.COLONGT(yytext, !pos,!pos));
<INITIAL>"#"         => (printf "2\n"; Tokens.HASH(yytext, !pos,!pos));

<INITIAL>"{"	 	 => (printf "2\n"; Tokens.LCURLY(yytext, !pos,!pos));
<INITIAL>"}"	 	 => (printf "2\n"; Tokens.RCURLY(yytext, !pos,!pos));

<INITIAL>"(*"		 => (printf "open comment ini\n"; YYBEGIN COMMENT; unmatched_comments := 1;
						 lex());
<INITIAL>"*)"		 => (printf "close comment ini\n"; error("Error: unmatched close comment", !pos, !pos); lex());

<COMMENT>"(*"		 => (printf "open comment com\n"; inc unmatched_comments; lex());
<COMMENT>"*)"		 => (printf "close comment com\n"; dec unmatched_comments; 
						 if (!unmatched_comments) = 0
						 then YYBEGIN INITIAL else (); 
						 lex());


<COMMENT>\n  		 => (printf "4\n"; pos := !pos + 1; lex());
<COMMENT>.  		 => (printf "4\n"; lex());

<INITIAL>"["	 	 => (printf "a\n"; Tokens.LBRACK(yytext, !pos,!pos));
<INITIAL>"]"	 	 => (printf "b\n"; Tokens.RBRACK(yytext, !pos,!pos));
<INITIAL>"("	 	 => (printf "c\n"; Tokens.LPAREN(yytext, !pos,!pos));
<INITIAL>")"	 	 => (printf "close paren\n"; Tokens.RPAREN(yytext, !pos,!pos));

<INITIAL>"if"	     => (printf "8\n"; Tokens.IF(yytext, !pos,!pos));
<INITIAL>"then"	     => (printf "8\n"; Tokens.THEN(yytext, !pos,!pos));
<INITIAL>"else"	     => (printf "8\n"; Tokens.ELSE(yytext, !pos,!pos));

<INITIAL>"while"	 => (printf "7\n"; Tokens.WHILE(yytext, !pos,!pos));
<INITIAL>"do"	     => (printf "7\n"; Tokens.DO(yytext, !pos,!pos));

<INITIAL>"let"	     => (printf "e\n"; Tokens.LET(yytext, !pos,!pos));
<INITIAL>"in"	     => (printf "e\n"; Tokens.IN(yytext, !pos,!pos));
<INITIAL>"end"	     => (printf "end\n"; Tokens.END(yytext, !pos,!pos));

<INITIAL>"orelse"    => (printf "5\n"; Tokens.ORELSE(yytext, !pos,!pos));
<INITIAL>"andalso"   => (printf "5\n"; Tokens.ANDALSO(yytext, !pos,!pos));

<INITIAL>"handle"    => (printf "5\n"; Tokens.HANDLE(yytext, !pos,!pos));
<INITIAL>"raise"	 => (printf "5\n"; Tokens.RAISE(yytext, !pos,!pos));
<INITIAL>"exception" => (printf "5\n"; Tokens.EXCEPTION(yytext, !pos,!pos));

<INITIAL>"val"		 => (printf "val\n"; Tokens.VAL(yytext, !pos, !pos));
<INITIAL>"and"		 => (printf "5\n"; Tokens.AND(yytext, !pos, !pos));
<INITIAL>"fn" 		 => (printf "5\n"; Tokens.FN(yytext, !pos, !pos));
<INITIAL>"fun"		 => (printf "fun\n"; Tokens.FUN(yytext, !pos, !pos));
<INITIAL>"case" 	 => (printf "5\n"; Tokens.CASE(yytext, !pos, !pos));
<INITIAL>"of"	     => (printf "5\n"; Tokens.OF(yytext, !pos, !pos));
<INITIAL>"_"		 => (printf "5\n"; Tokens.WILD(yytext, !pos, !pos));
<INITIAL>"op"		 => (printf "op\n"; Tokens.OP(yytext, !pos, !pos));
<INITIAL>"rec"		 => (printf "5\n"; Tokens.REC(yytext, !pos, !pos));

<INITIAL>"type" 	 => (printf "5\n"; Tokens.TYPE(yytext, !pos, !pos));
<INITIAL>"datatype"  => (printf "5\n"; Tokens.DATATYPE(yytext, !pos, !pos));
<INITIAL>"abstype"	 => (printf "5\n"; Tokens.ABSTYPE(yytext, !pos, !pos));
<INITIAL>"with"	 	 => (printf "5\n"; Tokens.WITH(yytext, !pos, !pos));
<INITIAL>"withtype"	 => (printf "5\n"; Tokens.WITHTYPE(yytext, !pos, !pos));
<INITIAL>"as"		 => (printf "5\n"; Tokens.AS(yytext, !pos, !pos));
<INITIAL>"open"		 => (printf "5\n"; Tokens.OPEN(yytext, !pos, !pos));
<INITIAL>"local"	 => (printf "5\n"; Tokens.LOCAL(yytext, !pos, !pos));
<INITIAL>"infix" 	 => (printf "5\n"; Tokens.INFIX(yytext, !pos, !pos));
<INITIAL>"infixr"	 => (printf "5\n"; Tokens.INFIXR(yytext, !pos, !pos));
<INITIAL>"nonfix"	 => (printf "5\n"; Tokens.NONFIX(yytext, !pos, !pos));

<INITIAL>"struct"	 => (printf "struct\n"; Tokens.STRUCT(yytext, !pos, !pos));
<INITIAL>"structure" => (printf "structure\n"; Tokens.STRUCTURE(yytext, !pos, !pos));
<INITIAL>"sig" 		 => (printf "5\n"; Tokens.SIG(yytext, !pos, !pos));
<INITIAL>"signature" => (printf "5\n"; Tokens.SIGNATURE(yytext, !pos, !pos));
<INITIAL>"functor"   => (printf "5\n"; Tokens.FUNCTOR(yytext, !pos, !pos));

<INITIAL>"include" 	 => (printf "5\n"; Tokens.INCLUDE(yytext, !pos, !pos));
<INITIAL>"where" 	 => (printf "5\n"; Tokens.WHERE(yytext, !pos, !pos));
<INITIAL>"eqtype" 	 => (printf "5\n"; Tokens.EQTYPE(yytext, !pos, !pos));
<INITIAL>"sharing" 	 => (printf "5\n"; Tokens.SHARING(yytext, !pos, !pos));

<INITIAL>"..."		 => (printf "5\n"; Tokens.DOTTED_WILDCARD(yytext, !pos, !pos));


<INITIAL>"(*!"       => (printf "5\n"; Tokens.LASSERT(yytext, !pos, !pos));
<INITIAL>"!*)"       => (printf "5\n"; Tokens.RASSERT(yytext, !pos, !pos));
<INITIAL>"REQUIRES"  => (printf "5\n"; req (!pos); Tokens.REQUIRES(yytext, !pos, !pos));
<INITIAL>"ENSURES"   => (printf "5\n"; ens (!pos); Tokens.ENSURES(yytext, !pos, !pos));


<INITIAL>{symbol}+   => (printf "symbol_token\n"; if yytext = ":" orelse
							yytext = "|" orelse
							yytext = "=" orelse
							yytext = "#" then REJECT()
						else Tokens.SYMBOLS(yytext, !pos, !pos));

<INITIAL>"'"{idchars}+  => (printf "quote_id\n"; Tokens.QUOTE_ID(yytext, !pos, !pos));

<INITIAL>{id}		    => (printf "token\n"; Tokens.ID(yytext, !pos, !pos));

<INITIAL>.              => (printf "smthing else\n"; error ("ignoring bad character "^yytext,!pos,!pos); lex());
