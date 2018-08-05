structure Memory :> MEMORY =
struct
  type 'a mem = 'a Seq.seq

  exception InvalidMem

(* ---------------------------------------------------------------------
    fun new:  int (1) * 'a -> 'a mem (2)
  
    REQUIRES: - int (1) >= 0
    ENSURES:  - n = int (1)
              - result equal to <'a (0), 'a (1), 'a (2),..., 'a (n - 1)>
    -------------------------------------------------------------------- *)

  fun new (n: int, a: 'a): 'a mem = if n = 0 then Seq.empty ()
      else if n < 0 then raise InvalidMem
      else Seq.tabulate (fn m => a) (n)

(* ---------------------------------------------------------------------
    fun read:  'a mem (1) -> int (2) -> 'a
  
    REQUIRES: - int (2) >= 0 and int (2) < Seq.length ('a mem (1))
    ENSURES:  - n = int (2)
              - result is the value in the Memory Sequence at index n.
    -------------------------------------------------------------------- *)

  fun read (A: 'a mem) (n: int): 'a = if n < 0 then raise InvalidMem
      else if n >= (Seq.length (A)) then raise InvalidMem
      else (Seq.nth (A) (n))

(* ---------------------------------------------------------------------
    fun write:  'a mem (1) -> (int (2) * 'a (3)) -> 'a mem (4)
  
    REQUIRES: - int (2) >= 0 and int (2) < Seq.length ('a mem (1))
    ENSURES:  - n == int (2)
              - Seq.nth ('a mem (4)) (n) = 'a (3)
    -------------------------------------------------------------------- *)

  fun write (A: 'a mem) (n: int, a: 'a): 'a mem = if n < 0 then raise InvalidMem
      else if n >= Seq.length (A) then raise InvalidMem 
      else (let
              val (Seq1, Seq2) = (Seq.take (A, n), Seq.drop (A, n + 1))
            in
              Seq.append (Seq.append (Seq1, Seq.singleton (a)), Seq2)
            end)

  fun toString (ts: 'a -> string) (M: 'a mem): string =
     Seq.toString (fn (i,x) => Int.toString i ^ ":" ^ ts x) (Seq.enum M)

  (* eq: ('a * 'a -> bool) -> ('a mem * 'a mem) -> bool
     eq a_eq (M1,M2) = b
     ENSURES: b == true iff M1 and M2 contain the same elements
              in the same order
   *)
  val eq = Seq.equal

  (* fromList: 'a list -> 'a mem
     fromList l = M
     ENSURES: M is a memory of the same size as l and containing
              the elements of l in the same order as l
   *)
  val fromList = Seq.fromList

  (* toList: 'a mem -> 'a list
     toList M = l
     ENSURES: l is the list of the elements of M in the same order
              as thei occur in M
   *)
  val toList = Seq.toList

end (* structure Memory *)
