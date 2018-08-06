(* andrew Id: ____sjahmad____ *)

datatype tree = empty
              | node of tree * string * tree

datatype treeC' = leafC of string
                | nodeC of treeC' * treeC'
datatype treeC  = emptyC
                | T of treeC'

(*!
   ENSURES: true
 !*)
fun inorder (empty: tree): string list = []
  | inorder (node(tL, c, tR)) = (inorder tL) @ (c :: inorder tR)

(*!
   ENSURES: true
!*)
fun size (empty: tree): int = 0
  | size (node(tL, _, tR)) = 1 + size tL + size tR

(*!
   ENSURES: true
 !*)
fun height (empty: tree): int = 0
  | height (node(tL, _, tR)) = 1 + Int.max(height tL, height tR)

(*!
   ENSURES: true
!*)
fun balanced (empty: tree) : bool = true
  | balanced (node(tL, _, tR)) =
    let
      val hl = height tL
      val hr = height tR
    in
      Int.abs(hl - hr) <= 1 andalso balanced tL andalso balanced tR
    end


(* If you need any function you defined in canonical.sml to define the
   following functions, copy it between the following comments *)

(* Begin code my canonical.sml *)

(*
  fun htraverseC: treeC' |----> string list
  REQUIRES: a canonical tree
  ENSURES: That the output string list is such that the string 
  on the left side at any node are appended before the right side of the node.
*)

fun htraverseC (leafC str: treeC'): string list = [str]
  | htraverseC (nodeC (a, b)) = htraverseC(a) @ htraverseC(b)

(*
  fun traverseC: treeC |----> string list
  REQUIRES: a treeC
  ENSURES: That the output string list is such that the string 
  on the left side at any node are appended before the right side of the node.
*)

fun traverseC (emptyC: treeC): string list = nil
  | traverseC (T x) = htraverseC(x)

val tree1: treeC = T (nodeC (leafC "nice", leafC "one"))
val ["nice", "one"] = traverseC (tree1)

(* End code from my canonical.sml *)


fun convert (emptyC: treeC): tree = empty
  | convert (T (nodeC (leafC a, leafC b))) = 
    node (empty, a, node(empty, b, empty))
  | convert (T (nodeC (leafC a, tR))) = node (empty, a, convert(T (tR)))
  | convert (T (nodeC (tL, leafC b))) = node (convert (T (tL)), b, empty)
  | convert (T (nodeC (tL, tR))) = node (convert(T(tL)), "", convert(T(tR)))

fun convert_safe (t: treeC): tree = if traverseC (t) <> inorder (convert t)
    then raise Fail "Order Not Maintained" 
        else convert t

fun splitN (empty: tree, i: int): tree * tree = (empty, empty)
  | splitN (node(tL, a, tR), i) = if i < size(tR) then let
    val (n1, n2) = splitN (tR, i)
  in
    (node (tL, a, n1), n2)  
  end
  else if i = size(tR) then (node (tL, a, empty), tR)
  else let
    val (n1, n2) = splitN (tL, i - size(tL) - 1)
  in
    (n1, node (n2, a, tR))
  end

fun getStr (node (tL, a, tR): tree): string = a

fun rightmost (empty: tree): string * tree = raise Fail "REQUIRES not met"
  | rightmost (t) = let
    val (t1, t2) = splitN (t, size(t) - 1)
  in
    (getStr(t2), t1)
  end

fun halves (empty: tree): tree * string * tree = 
    raise Fail "REQUIRES not met"
  | halves (node (tL, a, tR)) = let
    val (nL, nR) = splitN (node (tL, a, tR), (1 + size(tL) + size (tR)) div 2)
    val (str, newTree) = rightmost (nL)
  in
    (newTree, str, nR)
  end
    
fun rebalance (empty: tree): tree = empty
  | rebalance (node (tL, a, tR)) = let
    val (nL, b, nR) = halves (node (tL, a, tR))
  in
    node (rebalance nL, b, rebalance nR)
  end
