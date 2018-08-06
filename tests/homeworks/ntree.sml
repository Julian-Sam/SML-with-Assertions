structure NTree: NTREE =
struct

datatype 'a tree = Bud of 'a
                 | Branch of 'a tree list

(*
  fun Prod|- int list -> int 
  REQUIRES:  true
  ENSURES :  Multiplies all elements in the list   
*)

fun Prod ([]: int list): int = 1
  | Prod (x :: L) = x * Prod L 

val 24 = Prod [1, 2, 3, 4]

(*
  fun Sum|- int list -> int 
  REQUIRES:  true
  ENSURES :  Adds all elements in the list   
*)

fun Sum ([]: int list): int = 0
  | Sum (x :: L) = x + Sum L

val 10 = Sum [1, 2, 3, 4]

(*
  fun max|- int * int -> int
  REQUIRES:  True 
  ENSURES :  The maximum of the two numbers is returned. 
*)

fun max (n: int, m: int): int = if n > m then n else m

val 5 = max (1, 5)

(*
  fun height|- 'a tree -> int
  REQUIRES:  The input tree is of type 'a tree 
  ENSURES :  The height returned of the tree is correct. 
*)

fun height (Bud _: 'a tree): int = 1
  | height (Branch []) = 0
  | height (Branch L) =  1 + List.foldr (max) (0) (List.map (height) L)

val tree = Branch [Branch [Bud (43)], Bud (21), Bud (21)]
val 3 = height (tree)

(*
  fun makeBudsList|- int (1) -> 'a (2) -> 'a tree list -> 'a tree list
  REQUIRES:  true
  ENSURES :  A list of int (1) many buds who have the element 'a (2) in each of them 
*)

fun makeBudsList (0: int) (x: 'a) (acc: 'a tree list): 'a tree list = acc
  | makeBudsList m x acc = makeBudsList (m - 1) x (acc @ [Bud x])

val tree2 = [Bud 4, Bud 4, Bud 4]
val tree2 = makeBudsList 3 4 []

(*
  fun makeList|- int (1) -> 'a (2) -> 'a list -> 'a list
  REQUIRES:  true
  ENSURES :  A list of int (1) length with all 'a (2) elements
*)

fun makeList (0: int, x: 'a, acc: 'a list): 'a list = acc
  | makeList (m, x, acc) = makeList (m - 1, x, x :: acc)

val [2, 2, 2, 2, 2, 2] = makeList (6, 2, [])

(*
  fun fill|- int (1) * int (2) -> 'a -> 'a tree
  REQUIRES:  - int (1) >= 0
             - int (2) >= 0
  ENSURES :  Makes an 'a tree of height int (2) and each node has int (1) sub nodes. 
*)

fun fill (m: int, 0: int) (x: 'a): 'a tree = Branch []
  | fill (m, 1) x = Bud x
  | fill (m, 2) x = Branch (makeBudsList m x [])
  | fill (m, h) x = Branch (List.map (fill (m, h - 1)) (makeList (m, x, [])))
 
val Branch [Bud 12, Bud 12, Bud 12, Bud 12] = fill (4, 2) (12)

(*
  fun map|- ('a -> 'b) -> 'a tree -> 'b tree 
  REQUIRES:  true
  ENSURES :  All elements in 'a tree are mapped by the function.   
*)

fun map (f: 'a -> 'b) (Bud x: 'a tree): 'b tree = Bud (f(x))
  | map (f) (Branch L) = Branch (List.map (map f) (L))

val tree3 = Branch [Branch [Bud 36, Bud 23, Bud 12], Branch [Bud 23, Branch [Bud 45, Bud 10]], Bud 12]
val tree4 = Branch [Branch [Bud true, Bud false, Bud true], Branch [Bud false, Branch [Bud false, Bud true]], Bud true]

val tree4 = map (fn x => case x mod 2 of 0 => true | _ => false) (tree3)
(*
  fun filter|- ('a -> bool) -> 'a tree -> 'a tree 
  REQUIRES:  true
  ENSURES :  All elements in 'a tree that dont satisfy the function are removed.   
*)

fun filter (f: 'a -> bool) (Bud a: 'a tree): 'a tree = 
    if f(a) then Bud a else Branch nil
  | filter (f) (Branch L) = Branch (List.map (filter f) (L))

val tree5 = Branch [Branch [Bud 5, Bud 4, Bud 3], Bud 5]
val tree6 = Branch [Branch [Branch [], Bud 4, Bud 3], Branch []]
val tree6 = filter (fn 5 => false | _ => true) (tree5)

(*
  fun reduce|- ('a -> 'b) -> ('b list -> 'b) -> 'a tree -> 'b 
  REQUIRES:  true
  ENSURES :  The tree in reduced to one data type 'b.   
*)

fun reduce (f: 'a -> 'b) (g: 'b list -> 'b) (Bud a: 'a tree): 'b = f(a)
  | reduce f g (Branch L) = g (List.map (reduce f g) (L))

val tree7 = Branch [Branch [Bud 10, Bud 3], Branch [Bud 4, Bud 9], Bud 11]
val 37 = reduce (fn x => x) (Sum) (tree7)


(*
  fun inorder|- a tree -> 'a tree 
  REQUIRES:  true
  ENSURES :  All elements in 'a tree that dont satisfy the function are removed.   
*)

fun inorder (t: 'a tree): 'a list = 
	  reduce (fn a => [a]) (List.foldr (fn (e, L) => e @ L) ([])) (t)

val tree8 = Branch [Bud 12, Bud 12, Bud 12]
val [12, 12, 12] = inorder (tree8)

(*
  fun evanIntTree|- int list tree -> int 
  REQUIRES:  true
  ENSURES :  Product of all intBuds 
*)                                                                                                                                                       

fun evalIntTree (t: int list tree): int = 
    reduce (Prod) (Sum) (t) 

val tree9 = Branch [Branch [Bud ([4, 5, 6]), Bud ([1, 2])], Bud ([1, 2, 3])]
val 128 = evalIntTree (tree9)

(* Bonus *)

(*
  fun isTrueAll|- bool list -> bool
  REQUIRES:  true
  ENSURES :  Ensures all elements in bool list are true booleans. 
*)

fun isTrueAll ([]: bool list): bool = true
  | isTrueAll (x :: L) = if x = true 
                      then isTrueAll L
                      else false

val false = isTrueAll ([true, true, true, false])
val true = isTrueAll ([true, true])

(*
  fun isCanonical|- bool list -> bool
  REQUIRES:  true
  ENSURES :  That the tree inputted has no Branch nil amongst its inner nodes. 
*)

fun isCanonical (Branch []: 'a tree): bool = true
  | isCanonical (Bud a) = true
  | isCanonical (t: 'a tree) = reduce (fn a => true) 
              (fn [] => false | x => isTrueAll x) (t) 

val tree10 = Branch [Branch [], Branch [], Branch []]
val tree11 = Branch [Bud 12, Bud 34, Bud 8098]
val false  = isCanonical (tree10)
val true   = isCanonical (tree11)
val true   = isCanonical (Branch [])

(*
  fun mkCanonical|- 'a tree -> bool
  REQUIRES:  true
  ENSURES :  isCanonical (result)
*)

fun mkCanonical (t: 'a tree): 'a tree = if isCanonical (t) then t 
    else Branch (reduce (fn a => [Bud a]) 
                (List.foldr (fn (x, init) => case x
                      of [Branch []] => init 
                      | _ => x @ init) ([])) (t))

val tree12 = Branch [Branch [Bud 12, Branch []], Branch [], Bud 12]
val tree13 = Branch [Branch [Bud 12], Bud 12]
val tree13 = mkCanonical (tree12)

end (* structure NTree *)
