case (A) of
	(NONE, NONE) => NONE
	| (SOME (x), NONE) =>  SOME (x)
	| (NONE, SOME (y)) =>  SOME (y)
	| (SOME (x), SOME (y)) =>  SOME (x + y)

(*fun f () =
  let
    fun a () = b ()
    and b () = a ()
  in
    a ()
  end*)