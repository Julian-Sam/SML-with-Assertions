(* andrew Id: __sjahmad__ *)

functor MemoFib (D: EPHDICT where K = IntInfKey): FIB =
struct
  type t = IntInf.int

 (* ------------------------------------------------------
     fun fib: t (1) -> t
     REQUIRES: t (1) >= 0
     ENSURES: Let n = t (1)
              - nth fibonacci number is returned.
     ------------------------------------------------------ *)

  fun fib (n: t): t = let
  	val Dict = D.new ()
  	val _ = D.insert (Dict, (0, 0))
  	val _ = D.insert (Dict, (1, 1))

  	fun fib_helper (n: t): t = case D.lookup (Dict, n) of
  		SOME a => a
  		| NONE => let
			  			val y = fib_helper (n - 1) + fib_helper (n - 2)
			  			val _ = D.insert (Dict, (n, y))
			  	  in y end

  in
  	fib_helper (n)
  end

end (* functor MemoFib *)
