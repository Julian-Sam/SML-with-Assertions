functor Mlisa (structure Stream : ISTREAM
               structure Memory : MEMORY
	       structure Stack  : STACK) :> MLISA where I = Stream
                                                    and M = Memory
						    and S = Stack    =
struct
  structure S = Stack     (* Abbreviations *)
  structure M = Memory
  structure I = Stream

  datatype instruction = NOP                (* MLISA instructions *)
		       | STOP
		       | SS    of string * (int * int -> int)
		       | PUSHI of int
		       | PUSH  of int
		       | POP   of int
		       | BR    of string * (int * int -> bool) * int
		       | JUMP  of int
  
  datatype memcode = MNOP                   (* memory micro-codes *)
		   | LOAD
		   | STORE of int

  datatype stacode = STNOP                  (* stack micro-codes *)
		   | STPUSH of int

  type operation = int * stacode * memcode  (* micro-operations *)

  type instMem = instruction M.mem          (* instruction memory *)
  type stack   = int S.stack                (* stack *)
  type dataMem = int M.mem                  (* data memory *)
  type data    = stack * dataMem            (* data storage *)

  type state = data * (data, data) I.stream (* processor state *)

  exception Invalid       (* invalid operation -- should never happen *)
  exception Halt           (* end of execution -- used internally *)

(* ---------------------------------------------------------------------
    fun fetch:   instMem -> int (1) -> (data, instruction * data) I.stream
  
    REQUIRES: - int (1) >= 0 and int (1) < length (instMem)
    ENSURES:  - fetch returns a stream of values that map data variables
                (stack and memory) to the given instruction being fetched.
                These variables are altered based in the branching condition
                instructions.
    -------------------------------------------------------------------- *)

  fun fetch (iMem: instMem) (pc: int): (data, instruction * data) I.stream =
      let
        val Inst = M.read (iMem) (pc)

        fun fetch_helper ((s, mem): data): (data, instruction * data) I.front = 
            case Inst of
                STOP   => I.End
              | JUMP (addr) => I.Gen ((NOP, (s, mem)), fetch iMem addr)
              | BR (_, f, addr)  => (let
                val (v1, new_s) = S.pop (s)
                val (v2, new_s') = S.pop (new_s)
              in
                if f (v1, v2) then I.Gen ((NOP, (new_s', mem)), fetch iMem addr)
                else I.Gen ((NOP, (new_s', mem)), fetch (iMem) (pc + 1))
              end)
              | x      => I.Gen((x, (s, mem)), fetch iMem (pc + 1))
      in
        I.delay fetch_helper 
      end

(* ---------------------------------------------------------------------
    fun decode: (data, instruction * data) I.stream
  
    REQUIRES: - true
    ENSURES:  - decode takes the instructions from fetch and mappes the 
                inputted data according to the instruction read.
    -------------------------------------------------------------------- *)

  fun decode (D: (data, instruction * data) I.stream): 
                   (data, operation * data) I.stream =
      let   
        fun decode_helper (d: data): (data, operation * data) I.front =
            (let
                val exp = I.expose (D) (d)
              in
                (case exp of
                I.End => I.End
                | I.Gen ((inst, (s', mem')), D') =>

                          (case inst of
                           STOP      => raise Invalid
                         | BR   (_)  => raise Invalid
                         | JUMP (_)  => raise Invalid

                         | NOP       => I.Gen (((0, STNOP, MNOP), (s', mem')), decode (D'))

                         | SS (_, f) => (let
                                           val (v1, s'') = S.pop (s')
                                           val (v2, s''') = S.pop (s'')
                                         in
                                           I.Gen (((0, STPUSH (f(v1, v2)), MNOP), 
                                                       (s''', mem')), decode (D'))
                                        end)

                         | PUSHI (imm) => I.Gen (((0, STPUSH imm, MNOP), 
                                           (s', mem')), decode (D'))

                         | PUSH (addr) => I.Gen (((addr, STNOP, LOAD), 
                                                        (s', mem')), decode (D'))

                         | POP  (addr) => (let
                                             val (v1, s'') = S.pop (s')
                                           in
                                             I.Gen (((addr, STNOP, STORE v1), 
                                   (s'', mem')), decode (D'))
                                           end)))
                end)
      in
        I.delay decode_helper
      end

(* ---------------------------------------------------------------------
    fun mmu:  (data, operation * data) I.stream -> (data, data) I.stream
  
    REQUIRES: - true
    ENSURES:  - mmu takes the operations from decode and alters the memory
                and stack based on these micro operations.
    -------------------------------------------------------------------- *)

  fun mmu (oS: (data, operation * data) I.stream): (data, data) I.stream = let
      fun mmu_helper (d: data): (data, data) I.front = let
            val x = I.expose (oS) (d)
          in
            (case (x) of
                 I.End => I.End
               | I.Gen ((oP, (st, mem)), oS') =>
           (case (oP) of
           (_, STNOP, MNOP)      => I.Gen ((st, mem), mmu (oS'))
           | (addr, STNOP, m)    => (case (m) of 
                                     LOAD   => let
                                            val v1 = M.read (mem) (addr)
                                            val st' = S.push (v1, st)
                                          in
                                            I.Gen ((st', mem), mmu (oS'))
                                          end
   
                                   | STORE x => let
                                           val mem' = M.write (mem) (addr, x)
                                         in
                                           I.Gen ((st, mem'), mmu (oS'))
                                         end)
                 
           | (addr, STPUSH x, MNOP)   => I.Gen ((S.push (x, st), mem), mmu (oS'))))                                                              
          end
  in
    I.delay mmu_helper
  end

(* ---------------------------------------------------------------------
    fun connect: instMem -> int (1) -> stack -> dataMem -> state
  
    REQUIRES: - int (1) >= 0 and int (1) < length (instMem)
    ENSURES:  - pipelines the fetch decode and mmu functions so that they
                occur at once.
    -------------------------------------------------------------------- *)

  fun connect (iMem: instMem) (pc: int) (st: stack) (dMem: dataMem): state = let
      val fcS = fetch (iMem) (pc)
      val dS = decode (fcS)
      val mS = mmu (dS)

  in
    ((st,dMem), mS)
  end

(* ---------------------------------------------------------------------
    fun step: state -> state
  
    REQUIRES: - valid state 
    ENSURES:  - Performs one pipeline operation at a time and raises Halt
                when all operations are performed and instructions are read.
    -------------------------------------------------------------------- *)

  fun step ((d, mStream): state): state =
      case (I.expose (mStream) (d)) of
         I.End => raise Halt
        | I.Gen (data, mStream') => (data, mStream')

(* ---------------------------------------------------------------------
    fun simulate: state -> data
  
    REQUIRES: - valid state
    ENSURES:  - Gives final state of the stack and memory as a tuple 
                when all instructions have been evaluated and all 
                computation has been performed on storage.
    -------------------------------------------------------------------- *)

  fun simulate ((d,s): state): data = 
      simulate (step ((d,s)))
      handle Halt => d


  (* Printing and equality functions *)
  fun instruction_toString NOP          = "NOP"
    | instruction_toString STOP         = "STOP"
    | instruction_toString (SS (s,_))   = "SS "    ^ s
    | instruction_toString (PUSHI x)    = "PUSHI " ^ Int.toString x
    | instruction_toString (PUSH a)     = "PUSH "  ^ Int.toString a
    | instruction_toString (POP a)      = "POP "   ^ Int.toString a
    | instruction_toString (BR (s,_,i)) = "BR "    ^ s ^ " " ^ Int.toString i
    | instruction_toString (JUMP i)     = "JUMP "  ^ Int.toString i

  fun instruction_eq (NOP,          NOP)          = true
    | instruction_eq (STOP,         STOP)         = true
    | instruction_eq (SS (s1,_),    SS (s2,_))    = s1=s2
    | instruction_eq (PUSHI x1,     PUSHI x2)     = x1=x2
    | instruction_eq (PUSH a1,      PUSH a2)      = a1=a2
    | instruction_eq (POP a1,       POP a2)       = a1=a2
    | instruction_eq (BR (s1,_,i1), BR (s2,_,i2)) = s1=s2 andalso i1=i2
    | instruction_eq (JUMP i1,      JUMP i2)      = i1=i2
    | instruction_eq _ = false

  fun memcode_toString MNOP      = "MNOP"
    | memcode_toString LOAD      = "LOAD"
    | memcode_toString (STORE x) = "STORE " ^ Int.toString x

  fun memcode_eq (MNOP,     MNOP)     = true
    | memcode_eq (LOAD,     LOAD)     = true
    | memcode_eq (STORE x1, STORE x2) = x1=x2
    | memcode_eq _ = false

  fun stacode_toString STNOP      = "STNOP"
    | stacode_toString (STPUSH x) = "STPUSH " ^ Int.toString x

  fun stacode_eq (STNOP,     STNOP)     = true
    | stacode_eq (STPUSH x1, STPUSH x2) = x1=x2
    | stacode_eq _ = false

  fun operation_toString (addr, sc, mc) = "("  ^ Int.toString addr
					^ ", " ^ stacode_toString sc
					^ ", " ^ memcode_toString mc
					^ ")"

  fun operation_eq ((a1,sc1,mc1), (a2,sc2,mc2)) =
        a1=a1                andalso
	stacode_eq (sc1,sc2) andalso
	memcode_eq (mc1,mc2)

  fun data_toString (st,mem) =   "S: " ^ S.toString Int.toString st
                             ^ "; M: " ^ M.toString Int.toString mem

  fun data_eq ((st1,m1), (st2,m2)) =
        S.eq (op=) (st1,st2) andalso M.eq (op=) (m1,m2)

  fun state_toString l (d, dS) = "["  ^ data_toString d
			       ^ "; " ^ I.toString (data_toString, data_toString) l dS
			       ^ "]"

  fun state_eq (l: data list) ((d1,dS1): state, (d2,dS2): state): bool =
        data_eq (d1,d2) andalso I.eq data_eq l (dS1, dS2)

end (* functor Mlisa *)
