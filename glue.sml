(* sample.sml *)

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
       in SampleParser.parse(0,lexstream,print_error,())
      end

datatype ws_type = WS of int | NL of int | TAB of int;

fun readWS (ch_list: char list , len: int, ws_list: ws_type list) = 
  case (ws_list) of 
    nil => ch_list
  | typ :: ws_list' => case typ of
                         WS (num) =>  if num = len then #" " :: readWS (ch_list, len + 1, ws_list')
                                      else (case (ch_list) of
                                              nil => nil
                                            | ch :: ch_list' => ch :: readWS (ch_list', len + 1, ws_list))
                        | TAB (num) =>  if num = len then #"\t" :: readWS (ch_list, len + 1, ws_list')
                                        else (case (ch_list) of
                                                nil => nil
                                              | ch :: ch_list' => ch :: readWS (ch_list', len + 1, ws_list))
                        | NL (num) => if num = len then #"\n" :: readWS (ch_list, len + 1, ws_list')
                                      else (case (ch_list) of
                                              nil => nil
                                            | ch :: ch_list' => ch :: readWS (ch_list', len + 1, ws_list))

(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the sampleulator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse (filename) = 
      let val inStream = TextIO.openIn (filename)
          val lexer = SampleParser.makeLexer (fn _ =>
                                               (case TextIO.inputLine inStream
                                                of SOME s => s
                                                 | _ => ""))
    val dummyEOF = SampleLrVals.Tokens.EOF(0,0)
    fun loop lexer =
        let val (result,lexer) = invoke lexer
      val (nextToken,lexer) = SampleParser.Stream.get lexer
      val _ = case result
          of SOME r => (let
                          val charList = String.explode (r)
                          val newChar_List = readWS (charList, 0, sref_list)
                        in
                          print (String.implode(newChar_List))
                        end;
                        print "\n")
           | NONE => ()
         in if SampleParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end

end (* structure Sample *)