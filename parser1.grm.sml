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

fun lookup "bogus" = 10000
  | lookup s = 0

open String
val Conc: string ref = ref ""
val outStream = TextIO.openOut "testOutput.sml"


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\007\000\002\000\006\000\000\000\
\\001\000\004\000\000\000\011\000\000\000\000\000\
\\001\000\004\000\025\000\011\000\025\000\000\000\
\\022\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\010\000\008\000\000\000\
\\023\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\010\000\008\000\000\000\
\\024\000\001\000\007\000\002\000\006\000\003\000\005\000\000\000\
\\026\000\000\000\
\\027\000\000\000\
\\028\000\006\000\012\000\007\000\011\000\009\000\009\000\010\000\008\000\000\000\
\\029\000\009\000\009\000\010\000\008\000\000\000\
\\030\000\009\000\009\000\010\000\008\000\000\000\
\\031\000\005\000\013\000\006\000\012\000\007\000\011\000\008\000\010\000\
\\009\000\009\000\010\000\008\000\000\000\
\\032\000\006\000\012\000\007\000\011\000\009\000\009\000\010\000\008\000\000\000\
\\033\000\009\000\009\000\010\000\008\000\000\000\
\"
val actionRowNumbers =
"\005\000\002\000\004\000\000\000\
\\006\000\007\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\003\000\013\000\011\000\012\000\
\\010\000\009\000\008\000\001\000"
val gotoT =
"\
\\001\000\002\000\002\000\019\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\000\000\
\\000\000\
\\000\000\
\\001\000\013\000\000\000\
\\001\000\014\000\000\000\
\\001\000\015\000\000\000\
\\001\000\016\000\000\000\
\\001\000\017\000\000\000\
\\001\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 20
val numrules = 13
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
 | SEMICOLON of unit ->  (string) | CARAT of unit ->  (string)
 | REALDIV of unit ->  (string) | SUB of unit ->  (string)
 | INTDIV of unit ->  (string) | TIMES of unit ->  (string)
 | PLUS of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | NEWLINE of unit ->  (string)
 | START of unit ->  (int option) | EXP of unit ->  (int)
end
type svalue = MlyValue.svalue
type result = int option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 2) => true | (T 10) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
 $$ (T 0),nil
 $$ (T 2))::
(nil
,nil
 $$ (T 4))::
(nil
,nil
 $$ (T 5))::
(nil
,nil
 $$ (T 6))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "INT"
  | (T 2) => "PRINT"
  | (T 3) => "EOF"
  | (T 4) => "PLUS"
  | (T 5) => "TIMES"
  | (T 6) => "INTDIV"
  | (T 7) => "SUB"
  | (T 8) => "REALDIV"
  | (T 9) => "CARAT"
  | (T 10) => "SEMICOLON"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 0) => MlyValue.ID(fn () => ("bogus")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (EXP as EXP1) = EXP1 ()
 in (
print (Int.toString EXP);
                     print "\n";
                     SOME EXP
)
end)
 in ( LrTable.NT 1, ( result, PRINT1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671))
 => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1) =
 EXP1 ()
 in (SOME EXP)
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.START (fn _ => (NONE
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.NEWLINE NEWLINE1, NEWLINE1left, 
NEWLINE1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  NEWLINE1 = NEWLINE1 ()
 in (!Conc)
end)
 in ( LrTable.NT 1, ( result, NEWLINE1left, NEWLINE1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.EXP (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (let val _ = Conc := !Conc ^ Int.toString (INT) in 1 end)
end)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in (lookup ID)
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.PLUS PLUS1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1
 = EXP1 ()
 val  PLUS1 = PLUS1 ()
 val  EXP2 = EXP2 ()
 in (
let val _ = Conc := !Conc ^ Int.toString (EXP1) ^ " + " 
                         ^ Int.toString (EXP2)
                         in 1 end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.TIMES TIMES1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _
)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  
EXP1 = EXP1 ()
 val  TIMES1 = TIMES1 ()
 val  EXP2 = EXP2 ()
 in (
let val _ = Conc := !Conc ^ Int.toString (EXP1) ^ " * " 
                         ^ Int.toString (EXP2)
                         in 1 end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.INTDIV INTDIV1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left,
 _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  
EXP1 = EXP1 ()
 val  INTDIV1 = INTDIV1 ()
 val  EXP2 = EXP2 ()
 in (
let val _ = Conc := !Conc ^ Int.toString (EXP1) ^ " div " 
                         ^ Int.toString (EXP2)
                         in 1 end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.REALDIV REALDIV1, _, _)) :: ( _, ( MlyValue.EXP EXP1, 
EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  EXP1 = EXP1 ()
 val  REALDIV1 = REALDIV1 ()
 val  EXP2 = EXP2 ()
 in (
let val _ = Conc := !Conc ^ Int.toString (EXP1) ^ " / " 
                         ^ Int.toString (EXP2)
                         in 1 end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.SUB SUB1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _))
 :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1
 = EXP1 ()
 val  SUB1 = SUB1 ()
 val  EXP2 = EXP2 ()
 in (
let val _ = Conc := !Conc ^ Int.toString (EXP1) ^ " - " 
                         ^ Int.toString (EXP2)
                         in 1 end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( 
MlyValue.CARAT CARAT1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _
)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  
EXP1 = EXP1 ()
 val  CARAT1 = CARAT1 ()
 val  EXP2 = EXP2 ()
 in (
let fun e (m,0) = 1
                                | e (m,l) = m*e(m,l-1)
                             val _ = Conc := !Conc ^ Int.toString (e (EXP1, EXP2))
                         in 1 end
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.NEWLINE NEWLINE1, NEWLINE1left, 
NEWLINE1right)) :: rest671)) => let val  result = MlyValue.NEWLINE (fn
 _ => let val  NEWLINE1 = NEWLINE1 ()
 in (
let
                            val _ = TextIO.output (outStream, !Conc ^ "\n")
                            val _ = print (!Conc);
                            val _ = TextIO.flushOut (outStream)
                          in
                            !Conc
                          end
)
end)
 in ( LrTable.NT 2, ( result, NEWLINE1left, NEWLINE1right), rest671)

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
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.PLUS (fn () => i),p1,p2))
fun TIMES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.TIMES (fn () => i),p1,p2))
fun INTDIV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.INTDIV (fn () => i),p1,p2))
fun SUB (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.SUB (fn () => i),p1,p2))
fun REALDIV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.REALDIV (fn () => i),p1,p2))
fun CARAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.CARAT (fn () => i),p1,p2))
fun SEMICOLON (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.SEMICOLON (fn () => i),p1,p2))
end
end
