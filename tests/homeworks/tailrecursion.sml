(* andrew Id: ____sjahmad____ *)

(*
  fun mult: int list --> int
  REQUIRES: true 
  ENSURES : mult(L) = Summation of every integer in L.

*)

fun mult (nil: int list): int = 1
  | mult (x :: L) = x * mult (L)

val 6 = mult ([1, 2, 3])

(*
  fun mult: int list * int --> int
  REQUIRES: true 
  ENSURES : hmultTail(L, 1) = Summation of every integer in L.
*)

fun hmultTail (nil: int list, n: int): int = n
  | hmultTail (x :: L, n) = hmultTail (L, x*n)

val 6 = hmultTail ([1, 2, 3], 1)

(*
  fun multTail: int list --> int
  REQUIRES: true 
  ENSURES : multTail(L) = Summation of every integer in L.

*)

fun multTail (L: int list): int = hmultTail (L, 1)

val 6 = multTail ([1, 2, 3])
val 56 = multTail ([8,7,1])

(*
  fun hevenTail: int list * int list --> int * list
  REQUIRES: true 
  ENSURES : hevenTail(L, []) = Gives list of all values in L where x :: L = x mod 2
*)

fun hevenTail ([]: int list, l: int list): int list = l
  | hevenTail (x :: l, a) = hevenTail (l, a @ [x mod 2])

val [1,0,0,1,0] = hevenTail ([5,4,4,5,4], [])

(*
  fun evenTail: int list --> int * list
  REQUIRES: true 
  ENSURES : evenTail(L, []) = Gives list of all values in L where x :: L = x mod 2
*)

fun evenTail (l: int list): int list = hevenTail (l, [])

val [1,1,1,1,1,1] = evenTail([1,3,5,7,9,11])

(*
  fun even: int list --> int * list
  REQUIRES: true 
  ENSURES : even(L, []) = Gives list of all values in L where x :: L = x mod 2
*)

fun even ([]: int list): int list = []
  | even (x :: l) = [x mod 2] @ even(l)
  
val [1,0,0,0,0] = even ([5,2,4,6,8])

(*
  fun Trev: int list * int list --> int * list
  REQUIRES: true 
  ENSURES : List elements are reversed
*)

fun Trev([]: int list, l: int list): int list = l
  | Trev(x :: L, a) = Trev(L, x :: a)

val [1,2,3,4,5] = Trev ([5,4,3,2,1], [])

(*
  fun hevenTailFast: int list * int list --> int * list
  REQUIRES: true 
  ENSURES : hevenTailFast(L, []) = Gives list of all values in L where x :: L = x mod 2
*)

fun hevenTailFast ([]: int list, n: int list): int list = n
  | hevenTailFast (x :: L, a) = hevenTailFast (L, (x mod 2) :: a)

val [1,0,0,0,0] = even ([5,2,14,26,8])

(*
  fun hevenTail: int list --> int * list
  REQUIRES: true 
  ENSURES : hevenTail(L, []) = Gives list of all values in L where x :: L = x mod 2
*)

fun evenTailFast ([]: int list): int list = []
  | evenTailFast (L) = let
  	val l = hevenTailFast(L, nil)
  in
  	Trev(l, [])
  end

val [1,0,0,0,0] = even ([15,0,94,56,48])

datatype tree = Leaf of int
              | Node of tree * tree

(*
  fun treeMin: tree --> int
  REQUIRES: true 
  ENSURES : treeMin (t) = Gives lowest integer in tree t
*)

fun treeMin (Leaf x: tree): int = x
  | treeMin (Node(a, b)) = if treeMin a < treeMin b 
  then treeMin a else treeMin b

val treeA = Node (Leaf 12, Node (Node (Leaf 10, Leaf 123), Leaf 20))
val 10 = treeMin (treeA)