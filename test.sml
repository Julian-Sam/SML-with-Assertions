load"simpLib";
open Simplifier Simpsets arith_ss ;
infix ++;
trace_level := 5;
quotation := true; open Parse;

(* reduction in operand *)
SIMP_PROVE bool_ss [] (--`((P:'a) = P') ==> (P' = P)`--);

(* reduction in operand *)
SIMP_PROVE bool_ss [] (--`((P:'a) = P') ==> ((Q P:'b) = Q P')`--);

  fun last_elem (m: int, []: int list): bool = true
  	| last_elem (m, x :: l) = if m = x then last_elem(m, l)
  	  else false

  (*!
   REQUIRES:  true 
   ENSURES:   case (last_elem (m, lis)) of true  => (List.length (result) <> List.length (lis))
                                         | false   => (List.length (result) = List.length (lis)) 
  !*)

  fun removeTrailing (m: int, lis as []: int list): int list = []
  	| removeTrailing (m, lis as (x :: l)) = if m = x andalso last_elem (m, l) then []
  	  else [x] @ removeTrailing (m, l)  

  (*!
   REQUIRES:  true 
   ENSURES:   true 
  !*)

  fun ixmap_h (f: int * 'a -> 'b) (n: int) ([]: 'a list): 'b list = []
  	| ixmap_h f n (x :: l) = f (n, x) :: ixmap_h f (n + 1) l

  (*!
   REQUIRES: true 
   ENSURES: true
  !*)

  fun ixmap (f: int * 'a -> 'b) ([]: 'a list): 'b list = []
  	| ixmap f l = ixmap_h f 0 l

  (*!
   REQUIRES: true 
   ENSURES: true   
  !*)

  fun convolve_h (n: int, 0: int, acc: int, f: (int -> int), g: (int -> int)): int = 
  	  acc + (f (n) * g(0))
  	| convolve_h (n, x, acc, f, g) =  
  	  convolve_h (n + 1, x - 1, acc + (f (n) * g (x)), f, g)

  (*!
   REQUIRES: true 
   ENSURES: true 
  !*)

  fun convolve (n: int, f: (int -> int), g: (int -> int)): int = 
  	  convolve_h (0, n, 0, f, g)

  (* Test Cases *)

  val [] = removeTrailing(1, [1,1,1,1,1]) 

  val  [2, 2, 6, 12] = ixmap (fn (i, x) => 2*i + (round x)) [1.9, 0.2, 2.1, 5.6]

  val 80 = convolve(4, fn n: int => n+2, fn n: int => n*n)


end (* structure Util *)
