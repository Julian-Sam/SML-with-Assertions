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
          of SOME r =>
        (TextIO.output(TextIO.stdOut, r); TextIO.flushOut TextIO.stdOut)
           | NONE => ()
         in if SampleParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end

end (* structure Sample *)