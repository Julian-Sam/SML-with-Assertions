(* andrew Id: __sjahmad__ *)

functor MemoLCS (M: MEMOIZER where D.K = DNA2Key): LCS =
struct
open DNA

 (* --------------------------------------------------------------------
     fun lcs: DNA * DNA -> DNA
     REQUIRES: 
     ENSURES:  longest common subsequence between input DNAs is returned
     ------------------------------------------------------------------- *)

  fun lcs (a: DNA, b: DNA): DNA = let
  	fun lcs_helper (l: ((DNA * DNA) -> DNA)): ((DNA * DNA) -> DNA) = 
  		(fn (x, y) => (case (x, y) of
				 ([], _) => []
			   | (_, []) => []
			   | (x1 :: s1', x2 :: s2') => if Base.eq (x1, x2)
				   then x1 :: l (s1', s2')
				   else longer (l (x1 :: s1', s2'), 
				   			    l (s1', x2 :: s2'))))
  in
  	(M.memo (lcs_helper)) (a, b)
  end 

end (* functor MemoLCS *)
