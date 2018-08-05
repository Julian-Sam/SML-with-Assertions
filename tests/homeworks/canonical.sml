(* andrew Id: ___sjahmad____ *)

datatype treeC' = leafC of string
                | nodeC of treeC' * treeC'
datatype treeC  = emptyC
                | T of treeC'

datatype treeS = emptyS
               | leafS of string
               | nodeS of treeS * treeS


(* If you need any function you wrote in sloppy.sml to define the
   following functions, copy them between the following comments *)

(*********************** Begin code from sloppy.sml ***********************)

fun canonical (emptyS: treeS): bool = true
  | canonical (leafS str) = true
  | canonical (nodeS (emptyS, y)) = false
  | canonical (nodeS (x, emptyS)) = false
  | canonical (nodeS (x, y)) = canonical(x) andalso canonical(y)

fun simplify (emptyS: treeS): treeS = emptyS
  | simplify (leafS str) = leafS str
  | simplify (nodeS (emptyS, b)) = simplify(b)
  | simplify (nodeS (a, emptyS)) = simplify(a)
  | simplify (nodeS (a, b)) = if (simplify(a) = emptyS andalso simplify(b) = emptyS) then emptyS
   else if simplify(a) = emptyS then simplify(b)
   else if simplify(b) = emptyS then simplify(a)
   else nodeS (simplify(a), simplify(b))

fun traverseS (emptyS: treeS): string list = nil
  | traverseS (leafS str) = [str]
  | traverseS (nodeS (a,b)) = traverseS(a) @ traverseS(b)


(************************ End code from sloppy.sml ************************)

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
(*
  fun hconvertCan: treeS |----> treeC'
  REQUIRES: a treeS
  ENSURES: A canonical tree
*)


fun hconvertCan (emptyS: treeS): treeC' = raise Fail "Not Canonical Tree"
  | hconvertCan (leafS str) = leafC str
  | hconvertCan (nodeS (a, b)) = nodeC (hconvertCan(a), hconvertCan(b))

(*
  fun convertCan: treeS |----> treeC
  REQUIRES: a treeS
  ENSURES: A canonical tree
*)

fun convertCan (emptyS: treeS): treeC = emptyC
  | convertCan (leafS str) = T (leafC str)
  | convertCan (nodeS(a, b)) = T (nodeC (hconvertCan a, hconvertCan b)); 

val tree2: treeC = T (nodeC (leafC "nice", leafC "two"))
val tree2 = convertCan (nodeS (leafS "nice", leafS "two"))
(*
  fun convertCan_safe: treeS |----> treeC
  REQUIRES: a treeS
  ENSURES: A canonical tree but raises errors if the result is not canonical.
*)

fun convertCan_safe (t: treeS): treeC = if canonical(t) = false
	then raise Fail "REQUIRES spec not satisfied"
  else if traverseS t <> traverseC (convertCan(t)) 
  then raise Fail "ENSURES spec not satisfied"
  else convertCan(t) 

val tree3: treeC = T (nodeC (leafC "nice on", leafC "e two"))
val tree3 = convertCan_safe(nodeS (leafS "nice on", leafS "e two"))

(*
  fun hconvertSloppy: treeS |----> treeC'
  REQUIRES: a treeS
  ENSURES: A canonical tree
*)

fun hconvertSloppy ((emptyS): treeS): treeC' = 
  raise Fail "convertSloppy Not Implemented Properly"
  | hconvertSloppy (leafS str) = leafC str
  | hconvertSloppy (nodeS(a, b)) = 
    if simplify(a) = emptyS then hconvertSloppy(b)
    else if simplify(b) = emptyS then hconvertSloppy(a)
    else nodeC (hconvertSloppy(a), hconvertSloppy(b))

(*
  fun convertSloppy: treeS |----> treeC
  REQUIRES: a treeS
  ENSURES: A canonical tree
*)      

fun convertSloppy (emptyS: treeS): treeC = emptyC
  | convertSloppy (leafS str) = T (leafC str)
  | convertSloppy (nodeS (a, b)) = 
    if (simplify(a) = emptyS andalso simplify(b) = emptyS) then emptyC 
  	else if simplify(a) = emptyS then T (hconvertSloppy(b)) 
  	else if simplify(b) = emptyS then T (hconvertSloppy(a)) 
  	else T (nodeC (hconvertSloppy(simplify(a)), hconvertSloppy(simplify(b))))

val tree4: treeC = T (nodeC (leafC "nice on", nodeC (leafC "e two", leafC " three")))
val tree4 = convertSloppy (nodeS (nodeS (leafS "nice on", emptyS), nodeS (leafS "e two", leafS " three")))