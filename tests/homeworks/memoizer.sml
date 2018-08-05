(* andrew Id: ___sjahmad___ *)

functor Memoizer (D: EPHDICT): MEMOIZER =
struct
  structure D = D

 (* --------------------------------------------------------------
     fun memo: ((D.K.t -> 'a) -> (D.K.t -> 'a)) -> (D.K.t -> 'a)
     REQUIRES: The input function is in the valid format
     ENSURES: Let input f = g -> h
              - Gives a function that is a memoized representation 
                of function f.
     ------------------------------------------------------------- *)

  fun memo (f: (D.K.t -> 'a) -> (D.K.t -> 'a)): D.K.t -> 'a = 
  	let
      val memoTable: 'a D.dict = D.new ()

      fun memo_helper (k: D.K.t): 'a = 
      	  case (D.lookup (memoTable, k)) of
      	        SOME b => b 
      			| NONE => (let
                    val b = f (memo_helper) (k) 
      						  val _ = D.insert (memoTable, (k, b))
      			 		   in b end)
    in
    	memo_helper
	end

end (* functor Memoizer *)
