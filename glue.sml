(* This file provides glue code for building the sampleulator using the
 * parser and lexer specified in sample.lex and sample.grm.
*)

structure Sample : sig
             val parse : string -> unit
             val parse_print : string -> unit
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

  datatype ws_type = WS of int | NL of int | TAB of int | Comment of string * int;

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

				| Comment (str, num) => if num = len then readLines (ch_list, len + String.size (str), wslist', (String.explode (str)) @ final)              
										else (case (ch_list) of
                                       		    nil => (final, nil)
                                       		  | ch :: ch_list' => readLines (ch_list', len + 1, wslist, ch :: final))


  fun readFromLists (ws: int list, tb: int list, nl: int list, cm: (string * int) list, len: int, final: ws_type list) = 
    case (ws, tb, nl, cm) of
      
      (num1 :: ws', num2 :: tb', num3 :: nl', (str, num4) :: cm') => 

      						 if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                             else if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final) 
                             else if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                             else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                             else readFromLists (ws, tb, nl, cm, len + 1, final)



    | (nil, num2 :: tb', num3 :: nl', (str, num4) :: cm') => 

    				   if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final) 
                       else if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                       else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                       else readFromLists (ws, tb, nl, cm, len + 1, final)

    | (num1 :: ws', nil, num3 :: nl', (str, num4) :: cm') => 

    				   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                       else if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                       else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                       else readFromLists (ws, tb, nl, cm, len + 1, final)

    | (num1 :: ws', num2 :: tb', nil, (str, num4) :: cm') => 

    				   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                       else if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final)
                       else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                       else readFromLists (ws, tb, nl, cm, len + 1, final)

    | (num1 :: ws', num2 :: tb', num3 :: nl', nil)        => 

    				   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                       else if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final) 
                       else if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                       else readFromLists (ws, tb, nl, cm, len + 1, final)



    | (nil, nil, num3 :: nl', (str, num4) :: cm') => 

     			   if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                   else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)

    | (nil, num2 :: tb', nil, (str, num4) :: cm') => 

    			   if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final) 
                   else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)

    | (nil, num2 :: tb', num3 :: nl', nil) 		  => 

    			   if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final) 
                   else if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)



    | (num1 :: ws', nil, nil, (str, num4) :: cm') => 
    			   
    			   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                   else if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)

	| (num1 :: ws', nil, num3 :: nl', nil)		  => 

				   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                   else if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)    



    | (num1 :: ws', num2 :: tb', nil, nil) 		  => 

    			   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                   else if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)    


    | (num1 :: ws', nil, nil, nil)        => 

    			   if num1 = len then readFromLists (ws', tb, nl, cm, len + 1, WS (num1) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)    

    | (nil, num2 :: tb', nil, nil)		  => 

    			   if num2 = len then readFromLists (ws, tb', nl, cm, len + 1, TAB (num2) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)    

    | (nil, nil, num3 :: nl', nil)		  => 

    			   if num3 = len then readFromLists (ws, tb, nl', cm, len + 1, NL (num3) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)    

    | (nil, nil, nil, (str, num4) :: cm') =>

             if num4 = len then readFromLists (ws, tb, nl, cm', len + (String.size (str)), Comment (str, num4) :: final)
                   else readFromLists (ws, tb, nl, cm, len + 1, final)    




    | (nil, nil, nil, nil) => final


fun print_Ints (x) =
  case x of 
    nil => print ("[]\n\n")
  | i :: x' => (let
                   val _ = print (Int.toString (i) ^ " :: ")
                 in
                   print_Ints (x')
                 end)


  fun readList (x: char list, req: int list, ens: int list, cases: int list, num: int, final: char list) = 
    case (x) of 
      nil => final
    | ch :: x' => if ch = #"$" then (case (cases) of
                                       nil => raise Fail "Cant reach HERE! REQIRES"
                                     | num1 :: cases' => case (req) of inte :: req' => if num1 = num then readList (x', req, ens, cases', num, final @ (String.explode (Int.toString (inte))))
                                                                                       else readList (x, req', List.drop (ens, 1), cases, num + 1, final))
                  else if ch = #"%" then (case (cases) of
                                       nil => raise Fail "Cant reach HERE! ENSURES"
                                     | num1 :: cases' => case (ens) of inte :: ens' => if num1 = num then readList (x', req, ens, cases', num, final @ (String.explode (Int.toString (inte))))
                                                                                       else readList (x, List.drop (req, 1), ens', cases, num + 1, final))

                  else readList (x', req, ens, cases, num, final @ [ch])

  fun write_file filename content =
      let val fd = TextIO.openOut filename
          val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
          val _ = TextIO.closeOut fd
      in () end

  fun modify_filename filename = 
    String.substring (filename, 0, size(filename) - 4) ^ "_parsed" ^ ".sml";

  fun print_Cases (fun_name, cases) = 
  let
    val _ = print ("Function Name: " ^ fun_name ^ "\n\n");
  in
    case (cases) of
       nil => ()
    | (num, case_string) :: cases' => (let
                                        val _ = print ("Case Number: " ^ Int.toString (num) ^ "\n")
                                        val _ = print ("Case: " ^ case_string ^ "\n\n")
                                      in
                                        print_Cases (fun_name, cases')
                                      end)
  end

fun printing (cases) = 
  case (cases) of 
    nil => []
  | nice :: cases' => let 
                        val _ = print_Cases (nice)
                      in 
                        printing (cases')
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
                                           case TextIO.inputLine inStream
                                              of SOME s => s
                                               | _ => "") 

    val dummy_ws_list_ref: (int list * int list * int list * (string * int) list * (string * (int * string) list) list) ref = ref ([], [], [], [], [])
    val dummyEOF = SampleLrVals.Tokens.EOF(dummy_ws_list_ref, 0, 0)
    fun loop lexer =
        let val (result,lexer) = invoke lexer
      val (nextToken,lexer) = SampleParser.Stream.get lexer
      val _ = case result
          of SOME (x,y) => (let
                        val (ws_list, nl_list, tb_list, cm_list, assert_list) = y
                        val (ws_list', nl_list', tb_list', cm_list') = (List.rev (ws_list), List.rev (nl_list), List.rev (tb_list), List.rev (cm_list))
                        val combined_list = List.rev (readFromLists (ws_list', tb_list', nl_list', cm_list', 1, []))
                        val charList = String.explode (x)
                        val (charList', remaining) = readLines (charList, 2, combined_list, [])
                        val charList'' = List.rev(charList') @ remaining
                      in
                        write_file (modify_filename(filename)) (String.implode (charList''))
                      end)
           | NONE => ()
         in if SampleParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end



  fun parse_print (filename) = 
      let val inStream = TextIO.openIn (filename)
      val lexer = SampleParser.makeLexer (fn _ => 
                                           case TextIO.inputLine inStream
                                              of SOME s => s
                                               | _ => "") 

    val dummy_ws_list_ref: (int list * int list * int list * (string * int) list * (string * (int * string) list) list) ref = ref ([], [], [], [], [])
    val dummyEOF = SampleLrVals.Tokens.EOF(dummy_ws_list_ref, 0, 0)
    fun loop lexer =
        let val (result,lexer) = invoke lexer
      val (nextToken,lexer) = SampleParser.Stream.get lexer
      val _ = case result
          of SOME (x,y) => (let
                        val (ws_list, nl_list, tb_list, cm_list, assert_list) = y
                        val (req_list, ens_list, case_list) = (ws_list, nl_list, List.rev (tb_list))
                        val _ = print_Ints (case_list)
                        val new_char_list = readList (String.explode (x), req_list, ens_list, case_list, 1, [])
                      in
                        
                        print (String.implode (new_char_list) ^ "\n\n")
                      end)
           | NONE => ()
         in if SampleParser.sameToken(nextToken,dummyEOF) then ()
      else loop lexer
        end
       in loop lexer
      end


end