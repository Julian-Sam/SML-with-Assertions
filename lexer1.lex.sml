functor SampleLexFun(structure Tokens: Sample_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = print ( String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos) + 1; lex()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT(yytext, !pos, !pos))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INT(yytext, !pos, !pos))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.REAL(yytext, !pos, !pos))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WORD(yytext, !pos,!pos))
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.CHAR(yytext, !pos, !pos))
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.STRING(yytext, !pos, !pos))
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.UNEQUAL(yytext, !pos,!pos))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EQUALOP(yytext, !pos,!pos))
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.VARASSIGN(yytext, !pos,!pos))
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.GREATERTHAN(yytext, !pos,!pos))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LESSTHAN(yytext, !pos,!pos))
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.GREATERTHANEQUAL(yytext, !pos,!pos))
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LESSTHANEQUAL(yytext, !pos,!pos))
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TIMES(yytext, !pos,!pos))
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.REALDIV(yytext, !pos,!pos))
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.INTDIV(yytext, !pos,!pos))
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.PLUS(yytext, !pos,!pos))
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.SUB(yytext, !pos,!pos))
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.CARAT(yytext, !pos,!pos))
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.SEMICOLON (yytext, !pos, !pos))
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COMMA(yytext, !pos,!pos))
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ARROW(yytext, !pos,!pos))
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DARROW(yytext, !pos,!pos))
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BAR(yytext, !pos,!pos))
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COLON(yytext, !pos,!pos))
      end
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BANG(yytext, !pos,!pos))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COLONGT(yytext, !pos,!pos))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.COMPOSITION(yytext, !pos,!pos))
      end
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LBRACK(yytext, !pos,!pos))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RBRACK(yytext, !pos,!pos))
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LPAREN(yytext, !pos,!pos))
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RPAREN(yytext, !pos,!pos))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IF(yytext, !pos,!pos))
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.THEN(yytext, !pos,!pos))
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ELSE(yytext, !pos,!pos))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.WHILE(yytext, !pos,!pos))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.DO(yytext, !pos,!pos))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LET(yytext, !pos,!pos))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.IN(yytext, !pos,!pos))
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.END(yytext, !pos,!pos))
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ORELSE(yytext, !pos,!pos))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ANDALSO(yytext, !pos,!pos))
      end
fun yyAction44 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.HANDLE(yytext, !pos,!pos))
      end
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.RAISE(yytext, !pos,!pos))
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.EXCEPTION(yytext, !pos,!pos))
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID_NAME(yytext, !pos, !pos))
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error ("ignoring bad character "^yytext,!pos,!pos);
             lex())
      end
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ42(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"0"
              then yyAction4(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ42(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ42(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"~"
              then yyQ43(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ39(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ44(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ44(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ39(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ44(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                  else yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"F"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ41(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ41(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ41(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ41(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ41(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ41(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ41(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"/"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ15(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ37(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ15(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else yyAction37(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ48(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ47(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ46(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ45(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ51(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ50(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ49(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                      else yyAction45(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ55(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ54(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ53(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                      else yyAction42(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ60(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ59(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ58(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ57(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction29(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ56(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ62(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ61(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction40(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                      else yyAction40(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction40(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction40(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction40(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                      else yyAction34(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction34(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"a"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp = #"f"
                  then yyQ63(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"o"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ64(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction44(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                      else yyAction44(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction44(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyAction44(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction44(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction44(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyAction44(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ69(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ68(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ67(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ66(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ65(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction46(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction46(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ79(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ78(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ77(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ76(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"q"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"q"
              then if inp = #"p"
                  then yyQ75(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ74(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"c"
                  then yyQ73(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                      else yyAction41(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction41(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction41(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction41(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
                  else yyAction41(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction41, yyNO_MATCH))
              else yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ80(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction36(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyAction36(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction36(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction36(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ82(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ81(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"("
                      then yyAction47(strm, yyNO_MATCH)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                          else yyAction47(strm, yyNO_MATCH)
                    else if inp <= #"/"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"["
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #"["
                  then if inp <= #"@"
                      then yyAction47(strm, yyNO_MATCH)
                      else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"m"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"m"
                  then if inp = #"l"
                      then yyQ70(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ71(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"y"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"x"
                  then yyQ72(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"w"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"v"
                  then yyQ85(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"a"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp = #"i"
                  then yyQ83(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"p"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ84(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction43(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                      else yyAction43(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction43(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction43(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction43(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
                  else yyAction43(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ91(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ90(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ89(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ88(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ87(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction47(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                      else yyAction47(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction47(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction47(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ86(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ21(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ92(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ93(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ95(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ94(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ97(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ96(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ101(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ101(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ101(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ101(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ101(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ101(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ101(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ101(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ101(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ101(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ101(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ101(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ99(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"0"
              then yyAction5(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ99(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ99(strm', lastMatch)
            else if inp = #"x"
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"F"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"0"
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyQ38(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"E"
                  then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"w"
              then yyQ98(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"e"
                  then yyQ39(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ40(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ102(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') => yyQ104(strm', lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ106(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
and yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ106(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyQ4(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\f"
              then yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"\f"
              then if inp = #"\t"
                  then yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ22(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"+"
                  then yyQ9(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"+"
                  then if inp = #"!"
                      then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"!"
                      then if inp = #"\v"
                          then if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yyAction1(strm, yyNO_MATCH)
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ2(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                            else if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yyAction1(strm, yyNO_MATCH)
                        else if inp = #"\r"
                          then if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yyAction1(strm, yyNO_MATCH)
                        else if inp < #"\r"
                          then yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #" "
                          then yyQ1(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction1(strm, yyNO_MATCH)
                    else if inp = #"("
                      then yyQ6(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"("
                      then if inp = #"#"
                          then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"\""
                          then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction1(strm, yyNO_MATCH)
                    else if inp = #")"
                      then yyQ7(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ8(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #":"
                  then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #":"
                  then if inp = #"/"
                      then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"/"
                      then if inp = #"-"
                          then yyQ11(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #","
                          then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ12(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"0"
                      then yyQ14(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ15(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #">"
                  then yyQ20(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ18(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #";"
                      then yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= #"@"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction1(strm, yyNO_MATCH)
                  else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"m"
              then yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"d"
                  then yyQ26(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"d"
                  then if inp = #"_"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction1(strm, yyNO_MATCH)
                    else if inp < #"_"
                      then if inp = #"]"
                          then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"\\"
                          then if yyInput.eof(!(yystrm))
                              then UserDeclarations.eof(yyarg)
                              else yyAction1(strm, yyNO_MATCH)
                          else yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"a"
                      then yyQ25(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp <= #"`"
                      then if yyInput.eof(!(yystrm))
                          then UserDeclarations.eof(yyarg)
                          else yyAction1(strm, yyNO_MATCH)
                      else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ29(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"i"
                  then if inp = #"f"
                      then yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"f"
                      then yyQ27(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"h"
                      then yyQ28(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"l"
                  then yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"w"
              then yyQ34(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"r"
                  then yyQ32(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"r"
                  then if inp = #"o"
                      then yyQ31(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ33(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"}"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction1(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"{"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yyAction1(strm, yyNO_MATCH)
                else if inp = #"|"
                  then yyQ35(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"~"
              then yyQ36(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
