(*! 
	REQUIRES: true
	ENSURES: true
!*)
fun add (x: int) = x + 1
  | add (0) = 1

(*! 
	REQUIRES: true
	ENSURES: true
!*)
fun sub (x: int): int = x - 1
  | sub (0) = ("")
