(* andrew Id: ____sjahmad_____ *)

(*********************** Insertion sort on lists ***********************)

(* insert: int * int list -> int list
 * REQUIRES: sorted l
 * ENSURES: insert(x, l) = l', where
    - isPermutation (x::l, l')
    - sorted l'
 *)
fun insert (x: int, []: int list): int list = [x]
  | insert (x, y::l) =
     if x <= y
       then x::y::l
       else y :: insert (x, l)

(* isort: int list -> int list
 * ENSURES: isort l = l', where
    - isPermutation (l, l')
    - sorted l'
 *)
fun isort ([]: int list): int list = []
  | isort (x::l) = insert (x, isort l)


(*********************** Insertion sort on trees ***********************)

datatype 'a tree = empty
                 | node of 'a tree * 'a * 'a tree


(***** Begin utility functions *****)

(* sorted: int list -> bool
   ENSURES: sorted(l) = true iff l is sorted in ascending order
 *)
fun sorted ([]: int list): bool = true
  | sorted [_] = true
  | sorted (x::y::l) = x <= y andalso sorted (y::l)

(* inorder: 'a tree -> 'a list
 * ENSURES: inorder(t) is the inorder traversal of t
 *)
fun inorder (t: 'a tree): 'a list =
  let
    fun inorder' (empty: 'a tree, l: 'a list): 'a list = l
      | inorder' (node(tL, x, tR), l) =
          inorder'(tL, x :: inorder' (tR, l))
  in
    inorder' (t, [])
  end


(* Insert: int * int tree -> int tree
 * REQUIRES: sorted (inorder t)
 * ENSURES: Insert(x, t) = t', where
    - isPermutation (x::inorder t, inorder t')
    - sorted (inorder t')
 *)
fun Insert (x: int, empty: int tree): int tree = node(empty, x, empty)
  | Insert (x, node(tL,y,tR)) =
     if x <= y
       then node (Insert (x, tL), y, tR)
       else node (tL, y, Insert (x, tR))

(****** End utility functions ******)

(*
  fn createTree: int list * int tree -> int tree
  REQUIRES: true
  ENSURES: sorted (inorder result)
*)

fun createTree ([]: int list, tree1: int tree): int tree = tree1
  | createTree (x :: l, tree1) = createTree (l, Insert (x, tree1))

val empty = createTree ([], empty)
val Tree1 = node
    (node (node (empty,5,empty),6,empty),7,
     node (node (empty,8,empty),13,node (empty,16,empty)))
val Tree1 = createTree ([13,6,5,8,16], node (empty, 7, empty))

(*
  fn ILsort: int tree -> int tree
  REQUIRES: true
  ENSURES: sorted (inorder result)
*)

fun ILsort (empty: int tree): int tree = empty
  | ILsort t = createTree (inorder t, empty)

val empty = ILsort (empty)
val Tree2 = node
    (empty,1,
     node
       (node (node (node (empty,4,empty),12,empty),15,empty),34,
        node (empty,78,empty)))
val InputTree1 = node (node(node(empty, 1, empty), 34, 
    node(empty, 15, empty)), 12, node(empty, 4, node(empty, 78, empty)))
val Tree2 = ILsort (InputTree1)

(*
  fn Isort_Helper: int tree * int tree -> int tree
  REQUIRES: true
  ENSURES: sorted (inorder result)
*)

fun Isort_Helper (empty: int tree, tree1: int tree): int tree = tree1
  | Isort_Helper (node (tL, x, tR), tree1) = let
    val tree2 = Insert (x, tree1)
    val tree3 = Isort_Helper (tL, tree2) 
  in
    Isort_Helper (tR, tree3)
  end

val InputTree2 = node (node(empty, 4, empty), 0, node (empty, 6, empty))
val Tree3 = node (empty,0,node (empty,4,node (empty,6,empty)))
val Tree3 = Isort_Helper (InputTree2, empty)

(*
  fn Isort_Helper: int tree -> int tree
  REQUIRES: true
  ENSURES: sorted (inorder result)
*)

fun Isort (itree: int tree): int tree = Isort_Helper (itree, empty) 

val InputTree3 = node (node(empty, ~1, empty), ~45, node (empty, 56, empty))
val Tree4 = node (empty,~45,node (empty,~1,node (empty,56,empty)))
val Tree4 = Isort (InputTree3)