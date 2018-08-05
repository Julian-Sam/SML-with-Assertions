(* andrew Id: ___sjahmad___ *)

datatype tree = Empty
              | Leaf of real
              | Node of tree * tree

datatype itree = iEmpty
               | iLeaf of real
               | iNode of itree * int * itree
(* 
  fun max: int * int |----> int

  REQUIRES a,b to be int
  ENSURES max(a,b) computes and returns the larger of the two values.
*)

fun max(a: int, b: int): int = 
	if a >= b then a else b

val 2 = max(1, 2)
val 5 = max(~45, 5)
val ~12 = max(~12, ~32)

(*
  fun height: tree |----> int

  REQUIRES the input argument ot be a tree.
  ENSURES height(t) = height of tree t.
*)

fun height (Empty: tree): int = 0
  | height (Leaf l)      = 1
  | height (Node (a, b)) = 1 + max(height(a), height(b))

val 0 = height(Empty)
val 1 = height(Leaf 123.4)
val 4 = height(Node(Node(Leaf 1.0, Node(Leaf 45.67, Empty)),Node(Leaf 23.1, Leaf 9.80)))

(* 
  fun instrument: tree |----> itree

  REQUIRES the input argument ot be a tree.
  ENSURES instrument(t) returns the instrumented tree from the normal tree t
*)
fun instrument (Empty: tree): itree = iEmpty
  | instrument (Leaf l) = iLeaf l
  | instrument (Node(a, b)) = iNode(instrument(a), height(b) - height(a), 
    instrument(b))

val iEmpty = instrument (Empty)
val MadTree: tree = Node(Empty, Leaf 24.1)
val iMadTree: itree = iNode(iEmpty, 1, iLeaf 24.1)
val iMadTree = instrument (MadTree)

(*
  fun iHeight: itree |----> int

  REQUIRES the input argument ot be an itree.
  ENSURES iHeight(t) = height of tree t.
*)

fun iHeight (iEmpty: itree): int  = 0 
  | iHeight (iLeaf l) = 1
  | iHeight (iNode (a, i, b)) = 1 + max(iHeight(a), iHeight(b))

val 0 = iHeight(iEmpty)
val 1 = iHeight(iLeaf 24.5)
val iMadTree2: itree = iNode(iNode(iLeaf 14.8, ~1 ,iEmpty),~1,iLeaf 45.98)
val 3 = iHeight(iMadTree2)

(*
  fun validate: itree |----> bool

  REQUIRES the input argument ot be an itree.
  ENSURES validate(t) evaluates to true iff the degree of imbalance
  is properly measured at every node of the tree.
*)

fun validate (iEmpty: itree): bool = true 
  | validate (iLeaf _) = true
  | validate (iNode (a, i, b)) = if i = (iHeight(b) - iHeight(a)) 
  andalso validate(a) andalso validate(b) then true else false

val true = validate(iEmpty)
val true = validate(iLeaf 1.2)
val iMadTree3: itree = iNode(iNode(iLeaf 14.8, ~2 ,iEmpty),~1,iLeaf 46.0)
val iMadTree4: itree = iNode(iNode(iLeaf 14.8, ~1 ,iEmpty),~1,iLeaf 45.98)
val false = validate(iMadTree3)
val true = validate(iMadTree4)
(*
  fun tiltRight: itree |----> itree

  REQUIRES the input argument ot be an itree.
  ENSURES the degree of imbalance at every node of the itree is positive.
*)

fun tiltRight (iEmpty: itree): itree = iEmpty
  | tiltRight (iLeaf l) = iLeaf l
  | tiltRight (iNode (a, i, b)) = if i < 0 then 
  	iNode (tiltRight(b), ~i, tiltRight(a)) else 
  	iNode (tiltRight(a), i, tiltRight(b))

val iEmpty = tiltRight(iEmpty)
val iMadTree5 = iNode(iLeaf 23.5, 1,iNode(iEmpty, 1, iLeaf 34.2))
val iMadTree6 = iNode(iNode(iLeaf 34.2, ~1, iEmpty), ~1, iLeaf 23.5)
val iMadTree5 = tiltRight(iMadTree6)
(*
  fun iHeight: itree |----> int

  REQUIRES the input argument ot be an itree.
  ENSURES iHeight'(t) = height of itree t.
*)

fun iHeight' (iEmpty: itree): int = 0
  | iHeight' (iLeaf l) = 1
  | iHeight' (iNode (a, i, b)) = if i < 0 then 
  	1 + iHeight'(a) else 1 + iHeight'(b)

val 0 = iHeight(iEmpty);
val 1 = iHeight(iLeaf 11.1)
val iMadTree2: itree = iNode(iNode(iLeaf 15.6, ~1 ,iEmpty),~1,iLeaf 15.150)
val 3 = iHeight(iMadTree2)