(*! 
	REQUIRES: x < 20
	ENSURES: true
!*)
fun add (x as 0: int) = 1
  | add (x) = x + 1

(*! 
	REQUIRES: true
	ENSURES: true
!*)
fun sub (0: int): int = ~1
  | sub (x) = x - 1

