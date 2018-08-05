(* andrew Id: ____sjahmad_____ *)

structure Util :> UTIL =
struct

  (*
   fun last_elem | int * int list -> bool
   REQUIRES:  true 
   ENSURES:   Checks if the int is the last trailing in the list
  *)

  fun last_elem (m: int, []: int list): bool = true
  	| last_elem (m, x :: l) = if m = x then last_elem(m, l)
  	  else false

  (*
   fun removeTrailing | int (1) * int list -> int list
   REQUIRES:  true 
   ENSURES:   removes the int (1) is the last trailing in the list if it is the last trailing.
  *)

  fun removeTrailing (m: int, []: int list): int list = []
  	| removeTrailing (m, x :: l) = if m = x andalso last_elem (m, l) then []
  	  else [x] @ removeTrailing (m, l)  

  (*
   fun ixmap_h | (int * 'a -> 'b) -> int -> 'a list -> 'b list
   REQUIRES:  true 
   ENSURES:   Maps the 'a list to the 'b list using the function and the int value
  *)

  fun ixmap_h (f: int * 'a -> 'b) (n: int) ([]: 'a list): 'b list = []
  	| ixmap_h f n (x :: l) = f (n, x) :: ixmap_h f (n + 1) l

  (*
   fun ixmap | (int * 'a -> 'b) -> 'a list -> 'b list
   REQUIRES:  true 
   ENSURES:   Maps the 'a list to the 'b list using the function that it applies to each element
  *)

  fun ixmap (f: int * 'a -> 'b) ([]: 'a list): 'b list = []
  	| ixmap f l = ixmap_h f 0 l

  (*
   fun convolve_h | int * int * int * (int -> int) * (int -> int) -> int
   REQUIRES:  true 
   ENSURES:   Computes the accumulant from applying the two functions
  *)

  fun convolve_h (n: int, 0: int, acc: int, f: (int -> int), g: (int -> int)): int = 
  	  acc + (f (n) * g(0))
  	| convolve_h (n, x, acc, f, g) =  
  	  convolve_h (n + 1, x - 1, acc + (f (n) * g (x)), f, g)

  (*
   fun convolve | int * (int -> int) * (int -> int) -> int
   REQUIRES:  true 
   ENSURES:   Computes the accumulant from applying the two functions
  *)

  fun convolve (n: int, f: (int -> int), g: (int -> int)): int = 
  	  convolve_h (0, n, 0, f, g)

  (* Test Cases *)

  val [] = removeTrailing(1, [1,1,1,1,1]) 

  val  [2, 2, 6, 12] = ixmap (fn (i, x) => 2*i + (round x)) [1.9, 0.2, 2.1, 5.6]

  val 80 = convolve(4, fn n: int => n+2, fn n: int => n*n)


end (* structure Util *)















