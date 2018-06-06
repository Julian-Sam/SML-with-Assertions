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
open AbSyn

val hof_num = ref 0

fun nextNum () = 
  let
    val num = !hof_num
  in
    hof_num := !hof_num + 1;
    num
  end

exception argumentMismatch;
exception emptyDecList;

fun makeMatchList (nil) = raise emptyDecList
  | makeMatchList (L as (name,pat,exp)::t) =
    (name, List.map (fn (n,p,e) =>
               (if name <> n then (
                   print ("Syntax Error: Function definition with different names "
                   ^name^" and "^n^" not allowed.\n");
                   raise argumentMismatch)
                else MATCH(p,e))) L)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\018\000\065\000\
\\024\000\014\000\000\000\
\\001\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\019\000\082\000\
\\024\000\014\000\000\000\
\\001\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\
\\026\000\063\000\000\000\
\\001\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\
\\029\000\062\000\000\000\
\\001\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\
\\042\000\060\000\000\000\
\\001\000\012\000\079\000\000\000\
\\001\000\012\000\086\000\000\000\
\\001\000\014\000\013\000\017\000\012\000\022\000\011\000\023\000\010\000\
\\025\000\009\000\028\000\008\000\034\000\007\000\037\000\006\000\
\\041\000\005\000\043\000\004\000\000\000\
\\001\000\015\000\066\000\000\000\
\\001\000\016\000\083\000\000\000\
\\001\000\020\000\047\000\021\000\046\000\000\000\
\\001\000\030\000\042\000\031\000\041\000\032\000\040\000\033\000\039\000\
\\034\000\038\000\040\000\037\000\045\000\036\000\046\000\035\000\000\000\
\\001\000\034\000\072\000\000\000\
\\001\000\038\000\064\000\000\000\
\\001\000\044\000\026\000\000\000\
\\094\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\095\000\014\000\013\000\017\000\012\000\022\000\011\000\023\000\010\000\
\\025\000\009\000\028\000\008\000\034\000\007\000\037\000\006\000\
\\041\000\005\000\043\000\004\000\000\000\
\\096\000\013\000\029\000\000\000\
\\097\000\007\000\020\000\008\000\019\000\009\000\018\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\098\000\007\000\020\000\008\000\019\000\009\000\018\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\099\000\007\000\020\000\008\000\019\000\009\000\018\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\100\000\007\000\020\000\008\000\019\000\009\000\018\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\101\000\007\000\020\000\008\000\019\000\009\000\018\000\010\000\017\000\
\\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\102\000\024\000\014\000\000\000\
\\103\000\024\000\014\000\000\000\
\\104\000\024\000\014\000\000\000\
\\105\000\024\000\014\000\000\000\
\\106\000\024\000\014\000\000\000\
\\107\000\024\000\014\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\111\000\000\000\
\\112\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\113\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\114\000\024\000\014\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\024\000\014\000\000\000\
\\121\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\
\\035\000\084\000\000\000\
\\122\000\000\000\
\\123\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\
\\036\000\081\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\135\000\000\000\
\\136\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\000\000\
\\137\000\002\000\025\000\003\000\024\000\004\000\023\000\005\000\022\000\
\\006\000\021\000\007\000\020\000\008\000\019\000\009\000\018\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\024\000\014\000\
\\036\000\091\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\020\000\047\000\021\000\046\000\000\000\
\\142\000\000\000\
\"
val actionRowNumbers =
"\017\000\016\000\015\000\008\000\
\\008\000\018\000\008\000\008\000\
\\008\000\012\000\008\000\011\000\
\\012\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\031\000\005\000\040\000\008\000\
\\004\000\003\000\032\000\014\000\
\\038\000\050\000\049\000\051\000\
\\052\000\047\000\048\000\046\000\
\\045\000\001\000\009\000\059\000\
\\012\000\013\000\033\000\025\000\
\\024\000\029\000\028\000\027\000\
\\026\000\021\000\023\000\022\000\
\\020\000\019\000\030\000\036\000\
\\008\000\012\000\008\000\008\000\
\\008\000\060\000\057\000\006\000\
\\053\000\058\000\012\000\035\000\
\\037\000\043\000\002\000\010\000\
\\041\000\008\000\007\000\012\000\
\\008\000\039\000\008\000\054\000\
\\008\000\044\000\034\000\042\000\
\\055\000\013\000\056\000\000\000"
val gotoT =
"\
\\001\000\091\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\025\000\000\000\
\\002\000\026\000\000\000\
\\000\000\
\\002\000\028\000\000\000\
\\002\000\029\000\000\000\
\\002\000\030\000\000\000\
\\004\000\032\000\006\000\031\000\000\000\
\\002\000\041\000\000\000\
\\007\000\043\000\011\000\042\000\000\000\
\\004\000\046\000\006\000\031\000\000\000\
\\002\000\047\000\000\000\
\\002\000\048\000\000\000\
\\002\000\049\000\000\000\
\\002\000\050\000\000\000\
\\002\000\051\000\000\000\
\\002\000\052\000\000\000\
\\002\000\053\000\000\000\
\\002\000\054\000\000\000\
\\002\000\055\000\000\000\
\\002\000\056\000\000\000\
\\002\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\059\000\000\000\
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
\\007\000\043\000\011\000\065\000\000\000\
\\006\000\067\000\008\000\066\000\000\000\
\\009\000\069\000\013\000\068\000\000\000\
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
\\002\000\071\000\000\000\
\\004\000\072\000\006\000\031\000\000\000\
\\002\000\073\000\000\000\
\\002\000\074\000\000\000\
\\002\000\076\000\012\000\075\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\083\000\000\000\
\\000\000\
\\004\000\085\000\006\000\031\000\000\000\
\\002\000\086\000\000\000\
\\000\000\
\\002\000\076\000\012\000\087\000\000\000\
\\000\000\
\\002\000\088\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\090\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 92
val numrules = 55
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
 | ID of unit ->  (string) | STR of unit ->  (string)
 | REAL of unit ->  (string) | CHAR of unit ->  (string)
 | INT of unit ->  (string)
 | FUNMATCH of unit ->  ( ( string * pat * exp )  list)
 | EXPS of unit ->  (exp list) | DECs of unit ->  (dec list)
 | CONST of unit ->  (exp)
 | FUNBIND of unit ->  ( ( string * match list )  list)
 | VALBIND of unit ->  (dec) | DEC of unit ->  (dec)
 | PAT of unit ->  (pat) | PATs of unit ->  (pat list)
 | MATCHEXP of unit ->  (match list) | EXPC of unit ->  (exp list)
 | EXP of unit ->  (exp) | START of unit ->  (exp option)
end
type svalue = MlyValue.svalue
type result = exp option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "PLUS"
  | (T 2) => "SUB"
  | (T 3) => "INTDIV"
  | (T 4) => "REALDIV"
  | (T 5) => "TIMES"
  | (T 6) => "GREATERTHAN"
  | (T 7) => "LESSTHAN"
  | (T 8) => "GREATERTHANEQ"
  | (T 9) => "LESSTHANEQ"
  | (T 10) => "UNEQUALOP"
  | (T 11) => "EQUALOP"
  | (T 12) => "VARASSIGN"
  | (T 13) => "LET"
  | (T 14) => "IN"
  | (T 15) => "END"
  | (T 16) => "IF"
  | (T 17) => "THEN"
  | (T 18) => "ELSE"
  | (T 19) => "FUN"
  | (T 20) => "VAL"
  | (T 21) => "FN"
  | (T 22) => "RAISE"
  | (T 23) => "HANDLE"
  | (T 24) => "CASE"
  | (T 25) => "OF"
  | (T 26) => "AS"
  | (T 27) => "WHILE"
  | (T 28) => "DO"
  | (T 29) => "INT"
  | (T 30) => "CHAR"
  | (T 31) => "REAL"
  | (T 32) => "STR"
  | (T 33) => "ID"
  | (T 34) => "SEMICOLON"
  | (T 35) => "BAR"
  | (T 36) => "BANG"
  | (T 37) => "DARROW"
  | (T 38) => "COMMA"
  | (T 39) => "WILDCARD"
  | (T 40) => "LPAREN"
  | (T 41) => "RPAREN"
  | (T 42) => "LBRACK"
  | (T 43) => "RBRACK"
  | (T 44) => "TRUE"
  | (T 45) => "FALSE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
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
 in (SOME EXP)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.START (fn _ => (NONE
))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (Id ID)
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("+", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("-", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("*", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("div", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("/", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("<>", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("=", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP (">", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("<", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP (">=", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (INFIXEXP ("<=", EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 1, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( _, LBRACK1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => (Id "nil"))
 in ( LrTable.NT 1, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
RAISE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _
 => let val  EXP1 = EXP1 ()
 in (RAISEEXP (EXP1))
end)
 in ( LrTable.NT 1, ( result, RAISE1left, EXP1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.MATCHEXP MATCHEXP1, _, MATCHEXP1right)) ::
 _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val 
 result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (MATCHEXP as MATCHEXP1) = MATCHEXP1 ()
 in (HANDLEEXP (EXP, MATCHEXP))
end)
 in ( LrTable.NT 1, ( result, EXP1left, MATCHEXP1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( 
MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP
 (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in (ITE (EXP1, EXP2, EXP3))
end)
 in ( LrTable.NT 1, ( result, IF1left, EXP3right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (WHILEDO (EXP1, EXP2))
end)
 in ( LrTable.NT 1, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  EXP1 = EXP1 ()
 in (INFIXEXP (":=", Id (ID), EXP1))
end)
 in ( LrTable.NT 1, ( result, ID1left, EXP1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.MATCHEXP MATCHEXP1, _, MATCHEXP1right)) ::
 _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, CASE1left, _)) :: 
rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as 
EXP1) = EXP1 ()
 val  (MATCHEXP as MATCHEXP1) = MATCHEXP1 ()
 in (CASEOF (EXP, MATCHEXP))
end)
 in ( LrTable.NT 1, ( result, CASE1left, MATCHEXP1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.MATCHEXP MATCHEXP1, _, MATCHEXP1right)) :: 
( _, ( _, FN1left, _)) :: rest671)) => let val  result = MlyValue.EXP
 (fn _ => let val  (MATCHEXP as MATCHEXP1) = MATCHEXP1 ()
 in (FNOP (nextNum(), MATCHEXP))
end)
 in ( LrTable.NT 1, ( result, FN1left, MATCHEXP1right), rest671)
end
|  ( 23, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXPS EXPS1, _,
 _)) :: _ :: ( _, ( MlyValue.DECs DECs1, _, _)) :: ( _, ( _, LET1left,
 _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val 
 (DECs as DECs1) = DECs1 ()
 val  (EXPS as EXPS1) = EXPS1 ()
 in (List.hd (List.foldr (fn (x,y) => [LETSTAT(x,y)]) EXPS DECs))
end)
 in ( LrTable.NT 1, ( result, LET1left, END1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
BANG1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  EXP1 = EXP1 ()
 in (APPLY (Id ("!"), EXP1))
end)
 in ( LrTable.NT 1, ( result, BANG1left, EXP1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.EXPC (fn _ => let val  (EXP as EXP1) =
 EXP1 ()
 in ([EXP])
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.EXPC EXPC1, _, EXPC1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPC (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (EXPC as EXPC1) = EXPC1 ()
 in (EXP :: EXPC)
end)
 in ( LrTable.NT 2, ( result, EXP1left, EXPC1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.EXPS (fn _ => let val  (EXP as EXP1) =
 EXP1 ()
 in ([EXP])
end)
 in ( LrTable.NT 11, ( result, EXP1left, EXP1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXPS EXPS1, _, EXPS1right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPS (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (EXPS as EXPS1) = EXPS1 ()
 in (EXP :: EXPS)
end)
 in ( LrTable.NT 11, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.PAT PAT1, PAT1left, _)) :: rest671)) => let val  result = 
MlyValue.MATCHEXP (fn _ => let val  (PAT as PAT1) = PAT1 ()
 val  EXP1 = EXP1 ()
 in ([MATCH (PAT, EXP1)])
end)
 in ( LrTable.NT 3, ( result, PAT1left, EXP1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.MATCHEXP MATCHEXP1, _, MATCHEXP1right)) ::
 _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.PAT 
PAT1, PAT1left, _)) :: rest671)) => let val  result = 
MlyValue.MATCHEXP (fn _ => let val  (PAT as PAT1) = PAT1 ()
 val  EXP1 = EXP1 ()
 val  (MATCHEXP as MATCHEXP1) = MATCHEXP1 ()
 in (MATCH (PAT, EXP1) :: MATCHEXP)
end)
 in ( LrTable.NT 3, ( result, PAT1left, MATCHEXP1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.PAT (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (INTPAT (INT))
end)
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.CHAR CHAR1, CHAR1left, CHAR1right)) :: 
rest671)) => let val  result = MlyValue.PAT (fn _ => let val  (CHAR
 as CHAR1) = CHAR1 ()
 in (CHARPAT (CHAR))
end)
 in ( LrTable.NT 5, ( result, CHAR1left, CHAR1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.STR STR1, STR1left, STR1right)) :: rest671)
) => let val  result = MlyValue.PAT (fn _ => let val  (STR as STR1) = 
STR1 ()
 in (STRPAT (STR))
end)
 in ( LrTable.NT 5, ( result, STR1left, STR1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.REAL REAL1, REAL1left, REAL1right)) :: 
rest671)) => let val  result = MlyValue.PAT (fn _ => let val  (REAL
 as REAL1) = REAL1 ()
 in (REALPAT (REAL))
end)
 in ( LrTable.NT 5, ( result, REAL1left, REAL1right), rest671)
end
|  ( 35, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.PAT (fn _ => (BOOLPAT ("true")))
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 36, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.PAT (fn _ => (BOOLPAT ("false")))
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 37, ( ( _, ( _, WILDCARD1left, WILDCARD1right)) :: rest671)) =>
 let val  result = MlyValue.PAT (fn _ => (WILD))
 in ( LrTable.NT 5, ( result, WILDCARD1left, WILDCARD1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.PAT (fn _ => let val  (ID as ID1) = ID1 ()
 in (IDPAT (ID))
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.PAT PAT1, PAT1left, PAT1right)) :: rest671)
) => let val  result = MlyValue.PATs (fn _ => let val  (PAT as PAT1) =
 PAT1 ()
 in ([PAT])
end)
 in ( LrTable.NT 4, ( result, PAT1left, PAT1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.PATs PATs1, _, PATs1right)) :: _ :: ( _, ( 
MlyValue.PAT PAT1, PAT1left, _)) :: rest671)) => let val  result = 
MlyValue.PATs (fn _ => let val  (PAT as PAT1) = PAT1 ()
 val  (PATs as PATs1) = PATs1 ()
 in (PAT :: PATs)
end)
 in ( LrTable.NT 4, ( result, PAT1left, PATs1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.FUNMATCH FUNMATCH1, FUNMATCH1left, 
FUNMATCH1right)) :: rest671)) => let val  result = MlyValue.FUNBIND
 (fn _ => let val  (FUNMATCH as FUNMATCH1) = FUNMATCH1 ()
 in ([makeMatchList FUNMATCH])
end)
 in ( LrTable.NT 8, ( result, FUNMATCH1left, FUNMATCH1right), rest671)

end
|  ( 42, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.PAT PAT1, PAT1left, _)) :: rest671)) => let val  result = 
MlyValue.VALBIND (fn _ => let val  (PAT as PAT1) = PAT1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (VALB (PAT, EXP))
end)
 in ( LrTable.NT 7, ( result, PAT1left, EXP1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.PAT PAT1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.FUNMATCH (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (PAT as PAT1) = PAT1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ([(ID, PAT, EXP)])
end)
 in ( LrTable.NT 12, ( result, ID1left, EXP1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.FUNMATCH FUNMATCH1, _, FUNMATCH1right)) ::
 _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.PAT 
PAT1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) =>
 let val  result = MlyValue.FUNMATCH (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (PAT as PAT1) = PAT1 ()
 val  (EXP as EXP1) = EXP1 ()
 val  (FUNMATCH as FUNMATCH1) = FUNMATCH1 ()
 in ((ID, PAT, EXP) :: FUNMATCH)
end)
 in ( LrTable.NT 12, ( result, ID1left, FUNMATCH1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.VALBIND VALBIND1, _, VALBIND1right)) :: ( _
, ( _, VAL1left, _)) :: rest671)) => let val  result = MlyValue.DEC
 (fn _ => let val  (VALBIND as VALBIND1) = VALBIND1 ()
 in (VALBIND)
end)
 in ( LrTable.NT 6, ( result, VAL1left, VALBIND1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.FUNBIND FUNBIND1, _, FUNBIND1right)) :: ( _
, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.DEC
 (fn _ => let val  (FUNBIND as FUNBIND1) = FUNBIND1 ()
 in (FUNCTBs (FUNBIND))
end)
 in ( LrTable.NT 6, ( result, FUN1left, FUNBIND1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.DEC DEC1, DEC1left, DEC1right)) :: rest671)
) => let val  result = MlyValue.DECs (fn _ => let val  (DEC as DEC1) =
 DEC1 ()
 in ([DEC])
end)
 in ( LrTable.NT 10, ( result, DEC1left, DEC1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.DECs DECs1, _, DECs1right)) :: ( _, ( 
MlyValue.DEC DEC1, DEC1left, _)) :: rest671)) => let val  result = 
MlyValue.DECs (fn _ => let val  (DEC as DEC1) = DEC1 ()
 val  (DECs as DECs1) = DECs1 ()
 in (DEC :: DECs)
end)
 in ( LrTable.NT 10, ( result, DEC1left, DECs1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.CONST (fn _ => let val  (INT as INT1)
 = INT1 ()
 in (Int (INT))
end)
 in ( LrTable.NT 9, ( result, INT1left, INT1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.CHAR CHAR1, CHAR1left, CHAR1right)) :: 
rest671)) => let val  result = MlyValue.CONST (fn _ => let val  (CHAR
 as CHAR1) = CHAR1 ()
 in (Char (CHAR))
end)
 in ( LrTable.NT 9, ( result, CHAR1left, CHAR1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.STR STR1, STR1left, STR1right)) :: rest671)
) => let val  result = MlyValue.CONST (fn _ => let val  (STR as STR1)
 = STR1 ()
 in (Str (STR))
end)
 in ( LrTable.NT 9, ( result, STR1left, STR1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.REAL REAL1, REAL1left, REAL1right)) :: 
rest671)) => let val  result = MlyValue.CONST (fn _ => let val  (REAL
 as REAL1) = REAL1 ()
 in (Real (REAL))
end)
 in ( LrTable.NT 9, ( result, REAL1left, REAL1right), rest671)
end
|  ( 53, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.CONST (fn _ => (Bool ("true")))
 in ( LrTable.NT 9, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 54, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.CONST (fn _ => (Bool ("false")))
 in ( LrTable.NT 9, ( result, FALSE1left, FALSE1right), rest671)
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
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun INTDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun REALDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHANEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHANEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun UNEQUALOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun VARASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun RAISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun HANDLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun CHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.CHAR (fn () => i),p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.REAL (fn () => i),p1,p2))
fun STR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.STR (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun WILDCARD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end
