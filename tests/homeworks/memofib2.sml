(* andrew Id: ___sjahmad___ *)

functor MemoFib2 (M: MEMOIZER where D.K = IntInfKey): FIB =
struct
  type t = IntInf.int

 (* ------------------------------------------------------
     fun fib: t (1) -> t
     REQUIRES: t (1) >= 0
     ENSURES: Let n = t (1)
              - nth fibonacci number is returned.
     ------------------------------------------------------ *)


  fun fib (n: t): t = let
  	fun fib_helper (f: (t -> t)): (t -> t) = 
  		(fn a => if a > 1 
  					then (f (a - 1) + 
  						 f(a - 2))
  							else a)
  in
  	(M.memo (fib_helper)) (n)  
  end

end (* MemoFib2 *)
