(* This file provides glue code for building the SML parser using the
 * parser and lexer specified in lexer_engine.lex and parser_engine.grm.
*)

structure AssertEngine : sig
             val parse : string -> unit
             val parse_print : string -> unit
                 end = 
struct

(* 
 * We apply the functors generated from lexer_engine.lex and parser_engine.grm to produce
 * the AssertEngineParser structure.
 *)

  structure AssertEngineLrVals =
    AssertEngineLrValsFun(structure Token = LrParser.Token)

  structure AssertEngineLex =
    AssertEngineLexFun(structure Tokens = AssertEngineLrVals.Tokens)

  structure AssertEngineParser =
    Join(structure LrParser = LrParser
   structure ParserData = AssertEngineLrVals.ParserData
   structure Lex = AssertEngineLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
        TextIO.output(TextIO.stdOut,
          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in AssertEngineParser.parse(0,lexstream,print_error, ())
      end

(* Returns the next non whitespaced amount of characters *)
  fun check_string (x: char list, final: string) = 
      case (x) of
        nil => (final, x)
      | ch :: x' => if ch <> #" " andalso ch <> #"\n" andalso ch <> #"\t" then check_string (x', final ^ (String.implode ([ch])))
                    else (final, x)

(* Finds the placeholders of the requires and ensures line numbers and replaces them appropriately.*)
  fun readList (x: char list, req: int list, ens: int list, cases: int list, num: int, final: char list) = 
    case (x) of 
      nil => final
    | ch :: x' => if ch = #"$" then (let 
                                       val (str, x'') = check_string (x, "") 
                                     in 
                                       if str = "$#$$#$" then (case (cases) of
                                                                 nil            => raise Fail "Cant reach HERE! REQIRES"
                                                               | num1 :: cases' => case (req) of nil => [] | inte :: req' => if num1 = num then readList (x'', req, ens, cases', 
                                                                                                                 num, final @ (String.explode (Int.toString (inte))))
                                                                                                                 else readList (x, req', List.drop (ens, 1), cases, num + 1, final)) 
                                       else readList (x'', req, ens, cases, num, final @ String.explode (str))
                                     end)


             else if ch = #"%" then (let 
                                      val (str, x'') = check_string (x, "") 
                                     in   
                                       if str = "%#%%#%" then (case (cases) of
                                                                 nil            => raise Fail "Cant reach HERE! ENSURES"
                                                               | num1 :: cases' => case (ens) of nil => [] | inte :: ens' => if num1 = num then readList (x'', req, ens, cases', 
                                                                                                                 num, final @ (String.explode (Int.toString (inte))))
                                                                                                                 else readList (x, List.drop (req, 1), ens', cases, num + 1, final))
                                       else readList (x'', req, ens, cases, num, final @ String.explode (str))
                                     end)

             else readList (x', req, ens, cases, num, final @ [ch])

(* Writes the content to a file with the name filename. Returns a unit *)
  fun write_file filename content =
      let val fd = TextIO.openOut filename
          val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
          val _ = TextIO.closeOut fd
      in () end



(* Return index num option of substr in str if found or NONE otherwise *)
  fun index (str, substr) = let
    val (pref, suff) = Substring.position substr (Substring.full str)
    val (s, i, n) = Substring.base suff
  in
    if i = size str then
      NONE
    else
      SOME i
  end


(* Modifies the filename to have _parsed before the file extension *)

  fun modify_filename filename =
  	let
  		val position = valOf( index(filename, "."))
  		val first_part = String.substring (filename, 0, position)
  		val second_part = String.extract (filename, position, NONE)
  	in
    	first_part ^ "_parsed" ^ second_part
  	end


(* 
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the AssertEngineulator on the standard input and terminates when
 * an end-of-file is encountered. It writes the parsed content string to a new file. 
 *)

  fun parse (filename) = 
      let val inStream = TextIO.openIn (filename)
      val lexer = AssertEngineParser.makeLexer (fn _ => 
                                           case TextIO.inputLine inStream
                                              of SOME s => s
                                               | _ => "") 

    val dummy_ws_list_ref: (int list * int list * int list) ref = ref ([], [], [])
    val dummyEOF = AssertEngineLrVals.Tokens.EOF(dummy_ws_list_ref, 0, 0)
    fun loop lexer =
        let val (result,lexer) = invoke lexer
      val (nextToken,lexer) = AssertEngineParser.Stream.get lexer
      val _ = case result
          of SOME (x,y) => (let
                              val (req_list, ens_list, case_list) = y
                              val (req_list, ens_list, case_list) = (req_list, ens_list, List.rev (case_list))
                              val new_char_list = readList (String.explode (x), req_list, ens_list, case_list, 1, [])
                            in
                              write_file (modify_filename(filename)) (String.implode (new_char_list) ^ "\n\n")
                            end)
           | NONE => ()
         in if AssertEngineParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end


(* Like the parse function, but it prints instead of writing to astring *)
  fun parse_print (filename) = 
      let val inStream = TextIO.openIn (filename)
      val lexer = AssertEngineParser.makeLexer (fn _ => 
                                           case TextIO.inputLine inStream
                                              of SOME s => s
                                               | _ => "") 

    val dummy_ws_list_ref: (int list * int list * int list) ref = ref ([], [], [])
    val dummyEOF = AssertEngineLrVals.Tokens.EOF(dummy_ws_list_ref, 0, 0)
    fun loop lexer =
        let val (result,lexer) = invoke lexer
      val (nextToken,lexer) = AssertEngineParser.Stream.get lexer
      val _ = case result
          of SOME (x,y) => (let
                              val (req_list, ens_list, case_list) = y
                              val (req_list, ens_list, case_list) = (req_list, ens_list, List.rev (case_list))
                              val new_char_list = readList (String.explode (x), req_list, ens_list, case_list, 1, [])
                            in
                              print (String.implode (new_char_list) ^ "\n\n")
                            end)
           | NONE => ()
         in if AssertEngineParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end


end