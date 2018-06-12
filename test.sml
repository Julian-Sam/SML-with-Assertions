case (A) of
	(NONE, NONE) => NONE
	| (SOME (x), NONE) =>  SOME (x)
	| (NONE, SOME (y)) =>  SOME (y)
	| (SOME (x), SOME (y)) =>  SOME (x + y)

val (a:''x) (c:int) = 3;
=======
Control.Print.printDepth  := 50;
Control.Print.printLength := 50;
