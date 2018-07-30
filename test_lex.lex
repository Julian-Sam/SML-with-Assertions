structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
exception UnmatchedComments
val pos = ref 1

datatype ws_type = WS of int | NL of int | TAB of int | Comment of string * int


val ws_type_list_ref: (int list * int list * int list * (string * int) list) ref = ref ([], [], [], [])
val final_ref_list: (int list * int list * int list * (string * int) list * (string * (int * string) list) list) ref = ref ([], [], [], [], [])
val string_ref =  ref ("", 0)

fun ws num = let
			   val (ws_list, nl_list, tb_list, comm_list) = !ws_type_list_ref
			 in
			   ws_type_list_ref := (num :: ws_list, nl_list, tb_list, comm_list)
			 end

fun nl num = let
			   val (ws_list, nl_list, tb_list, comm_list) = !ws_type_list_ref
			 in
			   ws_type_list_ref := (ws_list, num :: nl_list, tb_list, comm_list)
			 end

fun tb num = let
			   val (ws_list, nl_list, tb_list, comm_list) = !ws_type_list_ref
			 in
			   ws_type_list_ref := (ws_list, nl_list, num :: tb_list, comm_list)
			 end

fun print_Ints (x) =
  case x of 
    nil => print ("[]\n\n")
  | i :: x' => (let
                   val _ = print (Int.toString (i) ^ " :: ")
                 in
                   print_Ints (x')
                 end)

fun rev_str (str: string) = String.implode (List.rev (String.explode (str)))

fun comm () = let
			   val (ws_list, nl_list, tb_list, comm_list) = !ws_type_list_ref
			   val (str, pos) = !string_ref
			   val str' = rev_str (str)
			 in
			   ws_type_list_ref := (ws_list, nl_list, tb_list, (str, pos) :: comm_list)
			 end

fun add_str (str1: string) = let
							  val (str2, pos) = !string_ref
							  val new_str = str1 ^ str2 
							in
							  string_ref := (new_str, pos)
							end

fun print_comments (cm) = 
	case (cm) of 
	  nil => ()
	| (str, pos) :: cm' => let
		val _ = print ("Comment Str = " ^ str ^ "\n\n" ^ "Starting Position = " ^ Int.toString (pos) ^ "\n\n")
	in
		print_comments (cm')
	end

fun add_placeHolder (s: string, len: int) = 
	case len of
	  0   => add_str (s)
	| num => add_placeHolder (s ^ "*", num - 1)


val unmatched_comments = ref 0
fun inc(x) = x := (!x + 1)
fun dec(x) = x := (!x - 1)

fun eof () = (if (!unmatched_comments) <> 0 
			 then (print("Error: Unmatched Comment Bracket"); raise UnmatchedComments)
			 else (let
					 val (ws, nl, tb, cm) = !ws_type_list_ref
					 val final_ref_list = ref (List.rev (ws), List.rev (nl), List.rev (tb), cm, [])
				   	 (*val _ = print_Ints (ws)
				   	 val _ = print_Ints (nl)*)
				   in
					 pos := 1;
					 ws_type_list_ref := ([], [], [], []);
					 Tokens.EOF(final_ref_list, !pos, !pos)
				   end))

fun error (e,l : int,_) = print (String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor SampleLexFun(structure Tokens: Sample_TOKENS));
%reject
%s COMMENT | ASSERTION; 
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
string_ = "\""{ascii}*"\"";
%%
<INITIAL>{space}     => (lex());
<INITIAL>{tab}       => (lex());
<INITIAL>{newline}   => (pos := !pos + 1; lex());

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
<INITIAL>"!"		 => (Tokens.BANG(yytext, !pos,!pos));
<INITIAL>":>"	     => (Tokens.COLONGT(yytext, !pos,!pos));
<INITIAL>"#"         => (Tokens.HASH(yytext, !pos,!pos));

<INITIAL>"{"	 	 => (Tokens.LCURLY(yytext, !pos,!pos));
<INITIAL>"}"	 	 => (Tokens.RCURLY(yytext, !pos,!pos));

<INITIAL>"(*"		 => (YYBEGIN COMMENT; unmatched_comments := 1;
						 string_ref := ("*(", yypos); lex());
<INITIAL>"*)"		 => (error("Error: unmatched close comment", !pos, !pos); lex());

<COMMENT>"(*"		 => (add_str ("*("); inc unmatched_comments; lex());
<COMMENT>"*)"		 => (add_str (")*"); dec unmatched_comments; 
						 if (!unmatched_comments) = 0
						 then (comm (); YYBEGIN INITIAL) else (); 
						 lex());

<COMMENT>\n  		 => (add_str ("\n"); pos := !pos + 1; print ("\n"); lex());
<COMMENT>.  		 => (add_str (yytext); lex());

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

<INITIAL>"(*!"       => (string_ref := ("**(", yypos); Tokens.LASSERT(yytext, !pos, !pos));
<INITIAL>"!*)"       => (add_str (")**"); comm(); Tokens.RASSERT(yytext, !pos, !pos));
<INITIAL>"REQUIRES"  => (ws (!pos); add_str ("********"); Tokens.REQUIRES(yytext, !pos, !pos));
<INITIAL>"ENSURES"   => (nl (!pos); add_str ("*******");  Tokens.ENSURES(yytext, !pos, !pos));




<INITIAL>{symbol}+   => (if yytext = ":" orelse
							yytext = "|" orelse
							yytext = "=" orelse
							yytext = "#" then REJECT()
						else Tokens.SYMBOLS(yytext, !pos, !pos));

<INITIAL>"'"{idchars}+  => (Tokens.QUOTE_ID(yytext, !pos, !pos));

<INITIAL>{id}		    => (Tokens.ID(yytext, !pos, !pos));

<INITIAL>.              => (error ("ignoring bad character "^yytext,!pos,!pos); lex());