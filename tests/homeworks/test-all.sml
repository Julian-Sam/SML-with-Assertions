functor TestAll (P : MLISA) =
struct
  structure M = P.M
  structure S = P.S
  structure I = P.I

  (* Include you tests here ... *)

  fun fact (n: int): int = let
    val dataMem = M.new (1, 1)
    val emptyS = S.empty ()
    val dataMem = M.write (dataMem) (0, n)
    val instMem = M.new (24, P.STOP)
    val instMem = M.write (instMem) (0, P.PUSH 0)
    val instMem = M.write (instMem) (1, P.PUSHI 1)
    val instMem = M.write (instMem) (2, P.PUSH 0)
    val instMem = M.write (instMem) (3, P.BR ("<=", (fn (a, b) => a <= b), 22))
    val instMem = M.write (instMem) (4, P.POP 0)
    val instMem = M.write (instMem) (5, P.PUSHI 1)
    val instMem = M.write (instMem) (6, P.PUSHI 1)
    val instMem = M.write (instMem) (7, P.PUSH 0)
    val instMem = M.write (instMem) (8, P.BR ("<=", (fn (a, b) => a <= b), 23))
    val instMem = M.write (instMem) (9, P.PUSH 0)
    val instMem = M.write (instMem) (10, P.PUSHI 1)
    val instMem = M.write (instMem) (11, P.PUSHI 1)
    val instMem = M.write (instMem) (12, P.PUSH 0)
    val instMem = M.write (instMem) (13, P.SS ("-", (fn (a, b) => a - b)))
    val instMem = M.write (instMem) (14, P.SS ("-", (fn (a, b) => a - b)))
    val instMem = M.write (instMem) (15, P.POP 0)
    val instMem = M.write (instMem) (16, P.PUSHI 1)
    val instMem = M.write (instMem) (17, P.PUSH 0)
    val instMem = M.write (instMem) (18, P.SS ("+", (fn (a, b) => a + b)))
    val instMem = M.write (instMem) (19, P.SS ("*", (fn (a, b) => a * b)))
    val instMem = M.write (instMem) (20, P.SS ("*", (fn (a, b) => a * b)))
    val instMem = M.write (instMem) (21, P.JUMP (6))
    val instMem = M.write (instMem) (22, P.PUSHI (1))
    val instMem = M.write (instMem) (23, P.STOP)
    val st = P.connect (instMem) (0) (emptyS) (dataMem)
    val (stk, data) = P.simulate (P.step (st))
    val (value, s) = S.pop (stk)
  in
    value
  end

  fun fib  _ = raise Fail "Unimplemented"

(*--------              TESTS CASES              --------*)

end (* functor TestAll *)


structure Test =
struct
  structure P = Mlisa (structure Stream = IStream
                       structure Memory = Memory
                       structure Stack = Stack (IStream))
  structure T = TestAll (P)
  open T

  (* ... or here *)

end (* structure Test *)
