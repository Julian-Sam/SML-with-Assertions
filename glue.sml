(* This file provides glue code for building the sampleulator using the
 * parser and lexer specified in sample.lex and sample.grm.
*)

structure Sample : sig
             val parse : string -> unit
                 end = 
struct

(* 
 * We apply the functors generated from sample.lex and sample.grm to produce
 * the SampleParser structure.
 *)

  structure SampleLrVals =
    SampleLrValsFun(structure Token = LrParser.Token)

  structure SampleLex =
    SampleLexFun(structure Tokens = SampleLrVals.Tokens)

  structure SampleParser =
    Join(structure LrParser = LrParser
   structure ParserData = SampleLrVals.ParserData
   structure Lex = SampleLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
        TextIO.output(TextIO.stdOut,
          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in SampleParser.parse(0,lexstream,print_error, ())
      end

datatype ws_type = WS of int | NL of int | TAB of int;

fun readLines (ch_list: char list , len: int, wslist: ws_type list, final: char list) = 
  case (wslist) of 
    nil => (final, ch_list)
  | typ :: wslist' => case typ of
              WS (num)  => if num = len then readLines (ch_list, len + 1, wslist', #" " :: final)
                               else (case (ch_list) of
                                       nil => (final, nil)
                                   | ch :: ch_list' => readLines (ch_list', len + 1, wslist, ch :: final))

              | TAB (num) => if num = len then readLines (ch_list, len + 1, wslist', #"\t" :: final)
                             else (case (ch_list) of
                                     nil => (final, nil)
                                     | ch :: ch_list' => readLines (ch_list', len + 1, wslist, ch :: final))
              
              | NL (num)  => if num = len then readLines (ch_list, len + 1, wslist', #"\n" :: final)
                             else (case (ch_list) of
                                     nil => (final, nil)
                                     | ch :: ch_list' => readLines (ch_list', len + 1, wslist, ch :: final))
             

fun readFromLists (ws: int list, tb: int list, nl: int list, len: int, final: ws_type list) = (print (""); print("");
  case (ws, tb, nl) of
    (num1 :: ws', num2 :: tb', num3 :: nl') => if num1 = len then readFromLists (ws', tb, nl, len + 1, WS (num1) :: final)
                           else if num2 = len then readFromLists (ws, tb', nl, len + 1, TAB (num2) :: final) 
                           else if num3 = len then readFromLists (ws, tb, nl', len + 1, NL (num3) :: final)
                           else readFromLists (ws, tb, nl, len + 1, final)

  | (nil, num2 :: tb', num3 :: nl') => if num2 = len then readFromLists (ws, tb', nl, len + 1, TAB (num2) :: final) 
                     else if num3 = len then readFromLists (ws, tb, nl', len + 1, NL (num3) :: final)
                     else readFromLists (ws, tb, nl, len + 1, final)

  | (num1 :: ws', nil, num3 :: nl') => if num1 = len then readFromLists (ws', tb, nl, len + 1, WS (num1) :: final)
                     else if num3 = len then readFromLists (ws, tb, nl', len + 1, NL (num3) :: final)
                     else readFromLists (ws, tb, nl, len + 1, final)

  | (num1 :: ws', num2 :: tb', nil) => if num1 = len then readFromLists (ws', tb, nl, len + 1, WS (num1) :: final)
                     else if num2 = len then readFromLists (ws, tb', nl, len + 1, TAB (num2) :: final) 
                     else readFromLists (ws, tb, nl, len + 1, final)

  | (nil, nil, num3 :: nl') => if num3 = len then readFromLists (ws, tb, nl', len + 1, NL (num3) :: final)
                 else readFromLists (ws, tb, nl, len + 1, final)

  | (nil, num2 :: tb', nil) => if num2 = len then readFromLists (ws, tb', nl, len + 1, TAB (num2) :: final) 
                 else readFromLists (ws, tb, nl, len + 1, final)

  | (num1 :: ws', nil, nil) => if num1 = len then readFromLists (ws', tb, nl, len + 1, WS (num1) :: final)
                 else readFromLists (ws, tb, nl, len + 1, final)

  | (nil, nil, nil) => final)

fun stringList (w: ws_type list) = 
  case w of
    nil => "[]"
  | typ :: w' => case typ of 
             WS  (num) =>  "WS (" ^ Int.toString(num) ^ ") :: " ^ (stringList w')
           | NL  (num) =>  "NL (" ^ Int.toString(num) ^ ") :: " ^ (stringList w')
           | TAB (num) => "TAB (" ^ Int.toString(num) ^ ") :: " ^ (stringList w')
          

(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the sampleulator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse (filename) = 
      let val inStream = TextIO.openIn (filename)
      val lexer = SampleParser.makeLexer (fn _ => 
                                           case TextIO.inputLine inStream
                                              of SOME s => s
                                               | _ => "") 

    val dummy_ws_list_ref: (int list * int list * int list) ref = ref ([], [], [])
    val dummyEOF = SampleLrVals.Tokens.EOF(dummy_ws_list_ref, 0, 0)
    fun loop lexer =
        let val (result,lexer) = invoke lexer
      val (nextToken,lexer) = SampleParser.Stream.get lexer
      val _ = case result
          of SOME (x,y) => (let
                        val (ws_list, nl_list, tb_list) = y
                        val (ws_list', nl_list', tb_list') = (List.rev (ws_list), List.rev (nl_list), List.rev (tb_list))
                        val combined_list = List.rev (readFromLists (ws_list', tb_list', nl_list', 1, []))
                        val charList = String.explode (x)
                        val (charList', remaining) = readLines (charList, 2, combined_list, [])
                        val charList'' = List.rev(charList') @ remaining
                      in
                        print (String.implode (charList'') ^ "\n")
                      end)
           | NONE => ()
         in if SampleParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end

end