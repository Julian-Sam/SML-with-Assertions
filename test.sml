fun memo (f: D.K.t -> 'a): D.K.t -> 'a =
  let
	val memoTable: 'a D.dict = D.new ()
	fun f_memoed (x: D.K.t): 'a =
	  case D.lookup (memoTable, x) of 
	  	SOME y => y
	  | NONE => let
	val y = f x
	val _ = D.insert (memoTable, (x,y))
  in y end
in
  f_memoed
end