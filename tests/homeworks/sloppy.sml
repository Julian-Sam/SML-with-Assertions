(* andrew Id: ____sjahmad____ *)

datatype treeS = emptyS
               | leafS of string
               | nodeS of treeS * treeS
(*
  fun traverseS: treeS |----> string list
  REQUIRES: input is a treeS
  ENSURES: That the output string list is such that the string 
  on the left side at any node are appended before the right side of the node. 
*)

fun traverseS (emptyS: treeS): string list = nil
  | traverseS (leafS str) = [str]
  | traverseS (nodeS (a,b)) = traverseS(a) @ traverseS(b)

val [] = traverseS (emptyS);
val ["a","be"] = traverseS(nodeS(leafS "a", leafS "be"))

(*
  fun canonical: treeS |----> bool
  REQUIRES: input is a treeS
  ENSURES: true if input treeS is just an emptyS or a treeS that consists
  of only nodeS and/or leafS.
*)

fun canonical (emptyS: treeS): bool = true
  | canonical (leafS str) = true
  | canonical (nodeS (emptyS, y)) = false
  | canonical (nodeS (x, emptyS)) = false
  | canonical (nodeS (x, y)) = canonical(x) andalso canonical(y)

val true = canonical(emptyS);
val true = canonical(nodeS(nodeS(leafS "hi",leafS "Sharjeel"),leafS "Khan"))
val false = canonical(nodeS(emptyS, leafS "RIP THIS TREE"))

(*
  fun simplify: treeS |----> treeS
  REQUIRES: input is a treeS
  ENSURES: that the output tree is canonical.
*)

fun simplify (emptyS: treeS): treeS = emptyS
  | simplify (leafS str) = leafS str
  | simplify (nodeS (emptyS, b)) = simplify(b)
  | simplify (nodeS (a, emptyS)) = simplify(a)
  | simplify (nodeS (a, b)) = 
   if (simplify(a) = emptyS andalso simplify(b) = emptyS) then emptyS
   else if simplify(a) = emptyS then simplify(b)
   else if simplify(b) = emptyS then simplify(a)
   else nodeS (simplify(a), simplify(b))

val emptyS = simplify(nodeS(nodeS(emptyS,emptyS),nodeS(emptyS,emptyS)))

(*
  fun simplify_safe: treeS |----> treeS
  REQUIRES: input is a treeS
  ENSURES: that the output tree is canonical and raises error if not.
*)

fun simplify_safe (t: treeS): treeS =
	if canonical(simplify(t)) = true then simplify(t)
	else raise Fail "ENSURES spec not satisfied"

val nodeS(leafS "hi", leafS "bye") = simplify_safe (nodeS (nodeS (leafS "hi", emptyS), nodeS (emptyS, leafS "bye")))