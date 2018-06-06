functor SampleLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Sample_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open String
open Int
val Conc: string ref = ref ""
val outStream = TextIO.openOut "testOutput.sml"

fun concatWith_(x, lis) = concatWith (x) (lis);


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\000\000\
\\001\000\001\000\014\000\008\000\013\000\013\000\012\000\032\000\011\000\
\\034\000\010\000\040\000\009\000\042\000\008\000\043\000\007\000\
\\044\000\006\000\045\000\005\000\000\000\
\\001\000\002\000\000\000\000\000\
\\001\000\003\000\051\000\004\000\035\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\009\000\055\000\010\000\031\000\012\000\054\000\
\\017\000\029\000\018\000\028\000\019\000\027\000\022\000\026\000\
\\023\000\025\000\024\000\024\000\025\000\023\000\026\000\022\000\
\\027\000\021\000\028\000\020\000\037\000\019\000\038\000\018\000\
\\039\000\017\000\046\000\016\000\000\000\
\\001\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\033\000\049\000\
\\037\000\019\000\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\001\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\035\000\048\000\
\\037\000\019\000\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\001\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\036\000\061\000\
\\037\000\019\000\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\001\000\009\000\052\000\000\000\
\\001\000\009\000\053\000\000\000\
\\001\000\014\000\050\000\000\000\
\\064\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\012\000\030\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\022\000\026\000\023\000\025\000\024\000\024\000\
\\025\000\023\000\026\000\022\000\027\000\021\000\028\000\020\000\
\\037\000\019\000\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\065\000\000\000\
\\066\000\001\000\014\000\008\000\013\000\013\000\012\000\032\000\011\000\
\\034\000\010\000\040\000\009\000\042\000\008\000\043\000\007\000\
\\044\000\006\000\045\000\005\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\003\000\051\000\004\000\035\000\005\000\034\000\006\000\033\000\
\\007\000\032\000\010\000\031\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\022\000\026\000\023\000\025\000\024\000\024\000\
\\025\000\023\000\026\000\022\000\027\000\021\000\028\000\020\000\
\\037\000\019\000\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\089\000\000\000\
\\090\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\012\000\054\000\017\000\029\000\018\000\028\000\
\\019\000\027\000\022\000\026\000\023\000\025\000\024\000\024\000\
\\025\000\023\000\026\000\022\000\027\000\021\000\028\000\020\000\
\\037\000\019\000\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\037\000\019\000\
\\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\037\000\019\000\
\\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\099\000\000\000\
\\100\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\037\000\019\000\
\\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\101\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\037\000\019\000\
\\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\102\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\037\000\019\000\
\\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\\103\000\004\000\035\000\005\000\034\000\006\000\033\000\007\000\032\000\
\\010\000\031\000\017\000\029\000\018\000\028\000\019\000\027\000\
\\022\000\026\000\023\000\025\000\024\000\024\000\025\000\023\000\
\\026\000\022\000\027\000\021\000\028\000\020\000\037\000\019\000\
\\038\000\018\000\039\000\017\000\046\000\016\000\000\000\
\"
val actionRowNumbers =
"\012\000\010\000\038\000\015\000\
\\017\000\016\000\014\000\001\000\
\\001\000\001\000\001\000\001\000\
\\013\000\001\000\018\000\000\000\
\\001\000\001\000\025\000\024\000\
\\023\000\022\000\021\000\020\000\
\\019\000\028\000\027\000\026\000\
\\011\000\033\000\031\000\032\000\
\\030\000\029\000\044\000\005\000\
\\004\000\009\000\034\000\007\000\
\\008\000\003\000\039\000\045\000\
\\046\000\047\000\001\000\001\000\
\\042\000\001\000\043\000\041\000\
\\001\000\040\000\006\000\049\000\
\\035\000\037\000\036\000\001\000\
\\048\000\002\000"
val gotoT =
"\
\\001\000\061\000\002\000\002\000\004\000\001\000\000\000\
\\003\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\004\000\034\000\000\000\
\\002\000\002\000\004\000\035\000\000\000\
\\002\000\002\000\004\000\036\000\000\000\
\\002\000\002\000\004\000\038\000\005\000\037\000\000\000\
\\002\000\002\000\004\000\041\000\005\000\040\000\006\000\039\000\000\000\
\\000\000\
\\002\000\002\000\004\000\042\000\000\000\
\\000\000\
\\008\000\043\000\000\000\
\\002\000\002\000\004\000\044\000\000\000\
\\002\000\002\000\004\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\003\000\013\000\000\000\
\\003\000\013\000\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\003\000\013\000\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\003\000\013\000\000\000\
\\002\000\002\000\004\000\054\000\000\000\
\\002\000\002\000\004\000\055\000\000\000\
\\000\000\
\\002\000\002\000\004\000\038\000\005\000\056\000\000\000\
\\000\000\
\\000\000\
\\002\000\002\000\004\000\058\000\006\000\057\000\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\003\000\013\000\000\000\
\\000\000\
\\000\000\
\\003\000\013\000\000\000\
\\002\000\002\000\004\000\060\000\000\000\
\\003\000\013\000\000\000\
\\000\000\
\"
val numstates = 62
val numrules = 40
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID_NAME of unit ->  (string) | REAL of unit ->  (string)
 | STRING of unit ->  (string) | CHAR of unit ->  (string)
 | WORD of unit ->  (string) | EXCEPTION of unit ->  (string)
 | RAISE of unit ->  (string) | HANDLE of unit ->  (string)
 | ANDALSO of unit ->  (string) | ORELSE of unit ->  (string)
 | ELSE of unit ->  (string) | THEN of unit ->  (string)
 | IF of unit ->  (string) | DO of unit ->  (string)
 | WHILE of unit ->  (string) | END of unit ->  (string)
 | IN of unit ->  (string) | LET of unit ->  (string)
 | LESSTHANEQUAL of unit ->  (string)
 | GREATERTHANEQUAL of unit ->  (string)
 | LESSTHAN of unit ->  (string) | GREATERTHAN of unit ->  (string)
 | VARASSIGN of unit ->  (string) | EQUALOP of unit ->  (string)
 | UNEQUAL of unit ->  (string) | COMPOSITION of unit ->  (string)
 | COLONGT of unit ->  (string) | BANG of unit ->  (string)
 | COLON of unit ->  (string) | BAR of unit ->  (string)
 | DARROW of unit ->  (string) | ARROW of unit ->  (string)
 | RBRACK of unit ->  (string) | LBRACK of unit ->  (string)
 | SEMICOLON of unit ->  (string) | CARAT of unit ->  (string)
 | REALDIV of unit ->  (string) | RPAREN of unit ->  (string)
 | LPAREN of unit ->  (string) | SUB of unit ->  (string)
 | INTDIV of unit ->  (string) | TIMES of unit ->  (string)
 | PLUS of unit ->  (string) | COMMA of unit ->  (string)
 | INT of unit ->  (string) | MATCH of unit ->  (string)
 | DEC of unit ->  (string) | EXP_SEQ of unit ->  (string)
 | EXP_LIST of unit ->  (string) | EXP of unit ->  (string)
 | ID of unit ->  (string) | CON of unit ->  (string)
 | START of unit ->  (string option)
end
type svalue = MlyValue.svalue
type result = string option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 3))::
(nil
,nil
 $$ (T 4))::
(nil
,nil
 $$ (T 5))::
(nil
,nil
 $$ (T 6))::
nil
val noShift = 
fn (T 1) => true | _ => false
val showTerminal =
fn (T 0) => "INT"
  | (T 1) => "EOF"
  | (T 2) => "COMMA"
  | (T 3) => "PLUS"
  | (T 4) => "TIMES"
  | (T 5) => "INTDIV"
  | (T 6) => "SUB"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "REALDIV"
  | (T 10) => "CARAT"
  | (T 11) => "SEMICOLON"
  | (T 12) => "LBRACK"
  | (T 13) => "RBRACK"
  | (T 14) => "ARROW"
  | (T 15) => "DARROW"
  | (T 16) => "BAR"
  | (T 17) => "COLON"
  | (T 18) => "BANG"
  | (T 19) => "COLONGT"
  | (T 20) => "COMPOSITION"
  | (T 21) => "UNEQUAL"
  | (T 22) => "EQUALOP"
  | (T 23) => "VARASSIGN"
  | (T 24) => "GREATERTHAN"
  | (T 25) => "LESSTHAN"
  | (T 26) => "GREATERTHANEQUAL"
  | (T 27) => "LESSTHANEQUAL"
  | (T 28) => "LET"
  | (T 29) => "IN"
  | (T 30) => "END"
  | (T 31) => "WHILE"
  | (T 32) => "DO"
  | (T 33) => "IF"
  | (T 34) => "THEN"
  | (T 35) => "ELSE"
  | (T 36) => "ORELSE"
  | (T 37) => "ANDALSO"
  | (T 38) => "HANDLE"
  | (T 39) => "RAISE"
  | (T 40) => "EXCEPTION"
  | (T 41) => "WORD"
  | (T 42) => "CHAR"
  | (T 43) => "STRING"
  | (T 44) => "REAL"
  | (T 45) => "ID_NAME"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1)
 = EXP1 ()
 in (SOME (EXP ^ "\n"))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.SEMICOLON SEMICOLON1, _, SEMICOLON1right))
 :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  
result = MlyValue.START (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  SEMICOLON1 = SEMICOLON1 ()
 in (SOME (EXP ^ ";\n"))
end)
 in ( LrTable.NT 0, ( result, EXP1left, SEMICOLON1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.START (fn _ => (NONE
))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.CON (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (INT)
end)
 in ( LrTable.NT 1, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.WORD WORD1, WORD1left, WORD1right)) :: 
rest671)) => let val  result = MlyValue.CON (fn _ => let val  (WORD
 as WORD1) = WORD1 ()
 in (WORD)
end)
 in ( LrTable.NT 1, ( result, WORD1left, WORD1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.REAL REAL1, REAL1left, REAL1right)) :: 
rest671)) => let val  result = MlyValue.CON (fn _ => let val  (REAL
 as REAL1) = REAL1 ()
 in (REAL)
end)
 in ( LrTable.NT 1, ( result, REAL1left, REAL1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.CHAR CHAR1, CHAR1left, CHAR1right)) :: 
rest671)) => let val  result = MlyValue.CON (fn _ => let val  (CHAR
 as CHAR1) = CHAR1 ()
 in (CHAR)
end)
 in ( LrTable.NT 1, ( result, CHAR1left, CHAR1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.CON (fn _ => let val  (
STRING as STRING1) = STRING1 ()
 in (STRING)
end)
 in ( LrTable.NT 1, ( result, STRING1left, STRING1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID_NAME ID_NAME1, ID_NAME1left, 
ID_NAME1right)) :: rest671)) => let val  result = MlyValue.ID (fn _ =>
 let val  (ID_NAME as ID_NAME1) = ID_NAME1 ()
 in (ID_NAME)
end)
 in ( LrTable.NT 2, ( result, ID_NAME1left, ID_NAME1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.UNEQUAL UNEQUAL1, UNEQUAL1left, 
UNEQUAL1right)) :: rest671)) => let val  result = MlyValue.ID (fn _ =>
 let val  (UNEQUAL as UNEQUAL1) = UNEQUAL1 ()
 in (UNEQUAL)
end)
 in ( LrTable.NT 2, ( result, UNEQUAL1left, UNEQUAL1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.EQUALOP EQUALOP1, EQUALOP1left, 
EQUALOP1right)) :: rest671)) => let val  result = MlyValue.ID (fn _ =>
 let val  (EQUALOP as EQUALOP1) = EQUALOP1 ()
 in (EQUALOP)
end)
 in ( LrTable.NT 2, ( result, EQUALOP1left, EQUALOP1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.VARASSIGN VARASSIGN1, VARASSIGN1left, 
VARASSIGN1right)) :: rest671)) => let val  result = MlyValue.ID (fn _
 => let val  (VARASSIGN as VARASSIGN1) = VARASSIGN1 ()
 in (VARASSIGN)
end)
 in ( LrTable.NT 2, ( result, VARASSIGN1left, VARASSIGN1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.GREATERTHAN GREATERTHAN1, GREATERTHAN1left,
 GREATERTHAN1right)) :: rest671)) => let val  result = MlyValue.ID (fn
 _ => let val  (GREATERTHAN as GREATERTHAN1) = GREATERTHAN1 ()
 in (GREATERTHAN)
end)
 in ( LrTable.NT 2, ( result, GREATERTHAN1left, GREATERTHAN1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.LESSTHAN LESSTHAN1, LESSTHAN1left, 
LESSTHAN1right)) :: rest671)) => let val  result = MlyValue.ID (fn _
 => let val  (LESSTHAN as LESSTHAN1) = LESSTHAN1 ()
 in (LESSTHAN)
end)
 in ( LrTable.NT 2, ( result, LESSTHAN1left, LESSTHAN1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.GREATERTHANEQUAL GREATERTHANEQUAL1, 
GREATERTHANEQUAL1left, GREATERTHANEQUAL1right)) :: rest671)) => let
 val  result = MlyValue.ID (fn _ => let val  (GREATERTHANEQUAL as 
GREATERTHANEQUAL1) = GREATERTHANEQUAL1 ()
 in (GREATERTHANEQUAL)
end)
 in ( LrTable.NT 2, ( result, GREATERTHANEQUAL1left, 
GREATERTHANEQUAL1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.LESSTHANEQUAL LESSTHANEQUAL1, 
LESSTHANEQUAL1left, LESSTHANEQUAL1right)) :: rest671)) => let val  
result = MlyValue.ID (fn _ => let val  (LESSTHANEQUAL as 
LESSTHANEQUAL1) = LESSTHANEQUAL1 ()
 in (LESSTHANEQUAL)
end)
 in ( LrTable.NT 2, ( result, LESSTHANEQUAL1left, LESSTHANEQUAL1right)
, rest671)
end
|  ( 16, ( ( _, ( MlyValue.BAR BAR1, BAR1left, BAR1right)) :: rest671)
) => let val  result = MlyValue.ID (fn _ => let val  (BAR as BAR1) = 
BAR1 ()
 in (BAR)
end)
 in ( LrTable.NT 2, ( result, BAR1left, BAR1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.COLON COLON1, COLON1left, COLON1right)) :: 
rest671)) => let val  result = MlyValue.ID (fn _ => let val  (COLON
 as COLON1) = COLON1 ()
 in (COLON)
end)
 in ( LrTable.NT 2, ( result, COLON1left, COLON1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.BANG BANG1, BANG1left, BANG1right)) :: 
rest671)) => let val  result = MlyValue.ID (fn _ => let val  (BANG as 
BANG1) = BANG1 ()
 in (BANG)
end)
 in ( LrTable.NT 2, ( result, BANG1left, BANG1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.PLUS PLUS1, PLUS1left, PLUS1right)) :: 
rest671)) => let val  result = MlyValue.ID (fn _ => let val  (PLUS as 
PLUS1) = PLUS1 ()
 in (PLUS)
end)
 in ( LrTable.NT 2, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.TIMES TIMES1, TIMES1left, TIMES1right)) :: 
rest671)) => let val  result = MlyValue.ID (fn _ => let val  (TIMES
 as TIMES1) = TIMES1 ()
 in (TIMES)
end)
 in ( LrTable.NT 2, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.SUB SUB1, SUB1left, SUB1right)) :: rest671)
) => let val  result = MlyValue.ID (fn _ => let val  (SUB as SUB1) = 
SUB1 ()
 in (SUB)
end)
 in ( LrTable.NT 2, ( result, SUB1left, SUB1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.INTDIV INTDIV1, INTDIV1left, INTDIV1right))
 :: rest671)) => let val  result = MlyValue.ID (fn _ => let val  (
INTDIV as INTDIV1) = INTDIV1 ()
 in (INTDIV)
end)
 in ( LrTable.NT 2, ( result, INTDIV1left, INTDIV1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.REALDIV REALDIV1, REALDIV1left, 
REALDIV1right)) :: rest671)) => let val  result = MlyValue.ID (fn _ =>
 let val  (REALDIV as REALDIV1) = REALDIV1 ()
 in (REALDIV)
end)
 in ( LrTable.NT 2, ( result, REALDIV1left, REALDIV1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.EXP_LIST (fn _ => let val  (EXP as 
EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP_LIST EXP_LIST1, _, EXP_LIST1right)) :: 
( _, ( MlyValue.COMMA COMMA1, _, _)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP_LIST (fn
 _ => let val  (EXP as EXP1) = EXP1 ()
 val  COMMA1 = COMMA1 ()
 val  (EXP_LIST as EXP_LIST1) = EXP_LIST1 ()
 in (EXP ^ ", " ^ EXP_LIST)
end)
 in ( LrTable.NT 4, ( result, EXP1left, EXP_LIST1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.EXP_SEQ (fn _ => let val  (EXP as EXP1
) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 5, ( result, EXP1left, EXP1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP_SEQ EXP_SEQ1, _, EXP_SEQ1right)) :: ( _
, ( MlyValue.SEMICOLON SEMICOLON1, _, _)) :: ( _, ( MlyValue.EXP EXP1,
 EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP_SEQ (fn
 _ => let val  (EXP as EXP1) = EXP1 ()
 val  SEMICOLON1 = SEMICOLON1 ()
 val  (EXP_SEQ as EXP_SEQ1) = EXP_SEQ1 ()
 in (EXP ^ ";\n " ^ EXP_SEQ)
end)
 in ( LrTable.NT 5, ( result, EXP1left, EXP_SEQ1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.CON CON1, CON1left, CON1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (CON as CON1) = 
CON1 ()
 in (CON)
end)
 in ( LrTable.NT 3, ( result, CON1left, CON1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = 
EXP1 ()
 val  (ID as ID1) = ID1 ()
 val  EXP2 = EXP2 ()
 in (concatWith_(" ", [EXP1, ID, EXP2]))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.EXP EXP1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (LPAREN as LPAREN1) = LPAREN1 ()
 val  (EXP as EXP1) = EXP1 ()
 val  (RPAREN as RPAREN1) = RPAREN1 ()
 in (concatWith_(" ", [LPAREN, EXP, RPAREN]))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.EXP_LIST EXP_LIST1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1
, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn
 _ => let val  (LPAREN as LPAREN1) = LPAREN1 ()
 val  (EXP_LIST as EXP_LIST1) = EXP_LIST1 ()
 val  (RPAREN as RPAREN1) = RPAREN1 ()
 in (concatWith_(" ", [LPAREN, EXP_LIST, RPAREN]))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.RBRACK RBRACK1, _, RBRACK1right)) :: ( _, (
 MlyValue.EXP_LIST EXP_LIST1, _, _)) :: ( _, ( MlyValue.LBRACK LBRACK1
, LBRACK1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn
 _ => let val  (LBRACK as LBRACK1) = LBRACK1 ()
 val  (EXP_LIST as EXP_LIST1) = EXP_LIST1 ()
 val  (RBRACK as RBRACK1) = RBRACK1 ()
 in (concatWith_(" ", [LBRACK, EXP_LIST, RBRACK]))
end)
 in ( LrTable.NT 3, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.RPAREN RPAREN1, _, RPAREN1right)) :: ( _, (
 MlyValue.EXP_SEQ EXP_SEQ1, _, _)) :: ( _, ( MlyValue.LPAREN LPAREN1, 
LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  (LPAREN as LPAREN1) = LPAREN1 ()
 val  (EXP_SEQ as EXP_SEQ1) = EXP_SEQ1 ()
 val  (RPAREN as RPAREN1) = RPAREN1 ()
 in (concatWith_(" ", [LPAREN, EXP_SEQ, RPAREN]))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( 
MlyValue.RAISE RAISE1, RAISE1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  (RAISE as RAISE1) = RAISE1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (concatWith_(" ", [RAISE, EXP]))
end)
 in ( LrTable.NT 3, ( result, RAISE1left, EXP1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.MATCH MATCH1, _, MATCH1right)) :: ( _, ( 
MlyValue.HANDLE HANDLE1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left,
 _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val 
 (EXP as EXP1) = EXP1 ()
 val  (HANDLE as HANDLE1) = HANDLE1 ()
 val  (MATCH as MATCH1) = MATCH1 ()
 in (concatWith_(" ", [EXP, HANDLE, MATCH]))
end)
 in ( LrTable.NT 3, ( result, EXP1left, MATCH1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.ANDALSO ANDALSO1, _, _)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  EXP1 = EXP1 ()
 val  (ANDALSO as ANDALSO1) = ANDALSO1 ()
 val  EXP2 = EXP2 ()
 in (concatWith_(" ", [EXP1, ANDALSO, EXP2]))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.ORELSE ORELSE1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left,
 _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  
EXP1 = EXP1 ()
 val  (ORELSE as ORELSE1) = ORELSE1 ()
 val  EXP2 = EXP2 ()
 in (concatWith_(" ", [EXP1, ORELSE, EXP2]))
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: ( _, ( 
MlyValue.ELSE ELSE1, _, _)) :: ( _, ( MlyValue.EXP EXP2, _, _)) :: ( _
, ( MlyValue.THEN THEN1, _, _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) ::
 ( _, ( MlyValue.IF IF1, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  (IF as IF1) = IF1 ()
 val  EXP1 = EXP1 ()
 val  (THEN as THEN1) = THEN1 ()
 val  EXP2 = EXP2 ()
 val  (ELSE as ELSE1) = ELSE1 ()
 val  EXP3 = EXP3 ()
 in (concatWith_(" ", [IF, EXP1, THEN, EXP2, ELSE, EXP3]))
end)
 in ( LrTable.NT 3, ( result, IF1left, EXP3right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.DO DO1, _, _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( 
MlyValue.WHILE WHILE1, WHILE1left, _)) :: rest671)) => let val  result
 = MlyValue.EXP (fn _ => let val  (WHILE as WHILE1) = WHILE1 ()
 val  (EXP as EXP1) = EXP1 ()
 val  (DO as DO1) = DO1 ()
 val  EXP2 = EXP2 ()
 in (concatWith_(" ", [WHILE, EXP, DO, EXP]))
end)
 in ( LrTable.NT 3, ( result, WHILE1left, EXP2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Sample_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.COMMA (fn () => i),p1,p2))
fun PLUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.PLUS (fn () => i),p1,p2))
fun TIMES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.TIMES (fn () => i),p1,p2))
fun INTDIV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.INTDIV (fn () => i),p1,p2))
fun SUB (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.SUB (fn () => i),p1,p2))
fun LPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.LPAREN (fn () => i),p1,p2))
fun RPAREN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.RPAREN (fn () => i),p1,p2))
fun REALDIV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.REALDIV (fn () => i),p1,p2))
fun CARAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.CARAT (fn () => i),p1,p2))
fun SEMICOLON (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.SEMICOLON (fn () => i),p1,p2))
fun LBRACK (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.LBRACK (fn () => i),p1,p2))
fun RBRACK (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.RBRACK (fn () => i),p1,p2))
fun ARROW (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.ARROW (fn () => i),p1,p2))
fun DARROW (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.DARROW (fn () => i),p1,p2))
fun BAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.BAR (fn () => i),p1,p2))
fun COLON (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.COLON (fn () => i),p1,p2))
fun BANG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.BANG (fn () => i),p1,p2))
fun COLONGT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.COLONGT (fn () => i),p1,p2))
fun COMPOSITION (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.COMPOSITION (fn () => i),p1,p2))
fun UNEQUAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.UNEQUAL (fn () => i),p1,p2))
fun EQUALOP (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.EQUALOP (fn () => i),p1,p2))
fun VARASSIGN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VARASSIGN (fn () => i),p1,p2))
fun GREATERTHAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.GREATERTHAN (fn () => i),p1,p2))
fun LESSTHAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.LESSTHAN (fn () => i),p1,p2))
fun GREATERTHANEQUAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26
,(ParserData.MlyValue.GREATERTHANEQUAL (fn () => i),p1,p2))
fun LESSTHANEQUAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.LESSTHANEQUAL (fn () => i),p1,p2))
fun LET (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.LET (fn () => i),p1,p2))
fun IN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.IN (fn () => i),p1,p2))
fun END (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.END (fn () => i),p1,p2))
fun WHILE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.WHILE (fn () => i),p1,p2))
fun DO (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.DO (fn () => i),p1,p2))
fun IF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.IF (fn () => i),p1,p2))
fun THEN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.THEN (fn () => i),p1,p2))
fun ELSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.ELSE (fn () => i),p1,p2))
fun ORELSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.ORELSE (fn () => i),p1,p2))
fun ANDALSO (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.ANDALSO (fn () => i),p1,p2))
fun HANDLE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.HANDLE (fn () => i),p1,p2))
fun RAISE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.RAISE (fn () => i),p1,p2))
fun EXCEPTION (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.EXCEPTION (fn () => i),p1,p2))
fun WORD (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.WORD (fn () => i),p1,p2))
fun CHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.CHAR (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.REAL (fn () => i),p1,p2))
fun ID_NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.ID_NAME (fn () => i),p1,p2))
end
end
