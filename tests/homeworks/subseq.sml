
(*
  fn lenSeq: ''a list * int -> int
  REQUIRES: true
  ENSURES: result represents number of elements in the generic list
*)

fun lenSeq ([]: ''a list, acc: int): int = acc
  | lenSeq (x :: l, acc) = lenSeq (l, 1 + acc) 

val 4 = lenSeq ([1, 2, 3, 4], 0)
val 0 = lenSeq ([], 0)

(*
  fn findMaxLen: (int * ''a list) list * (int * ''a list) -> (int * ''a list)
  REQUIRES: Let (x, y) represent (int * ''a list)
            - lenSeq (y) == x
  ENSURES: Returns pair with the highest length.
*)

fun findMaxLen ([]: (int * ''a list) list, p1: (int * ''a list)): 
	(int * ''a list) = p1
  | findMaxLen ((len1, xL) :: l, (len2, xL2)) =	
    if len1 > len2 then findMaxLen (l, (len1, xL))
  	else findMaxLen (l, (len2, xL2))

val (3,[4,5,6]) = findMaxLen ([(2, [1, 2]), (3, [4, 5, 6])], (1, [1]))

(*
  fn lcs: ''a list * ''a list -> ''a list
  REQUIRES: true
  ENSURES: Returns largest common subsequence
*)

fun lcs ([]: ''a list, []: ''a list): ''a list = []
  |	lcs ([], l2) = []
  | lcs (l1, []) = []
  | lcs (x :: l1, y :: l2) = if x = y 
  	then x :: lcs(l1, l2)
	else let
		val seq2 = lcs (l1, y :: l2)
		val seq3 = lcs (x :: l1, l2)
		val len2 = lenSeq (seq2, 0)
		val len3 = lenSeq (seq3, 0)
		val lenL = [(len2, seq2)] @ [(len3, seq3)]
		val (len, seq) = findMaxLen (lenL, (len2, seq2))
	in
		seq
	end

val [4, 5 ,7] = lcs ([3, 4, 5, 7], [4, 3, 5, 7])

exception SubSeqTooLong;
exception SubSeqTooShort;

(*
  fn css: ''a list * ''a list * int -> ''a list
  REQUIRES: true
  ENSURES: Returns common subsequence = int k passed in as argument.
          - If largest common subsequence is less than size k 
            then raises exception SubSeqTooShort
*)

fun css (l1: ''a list, []: ''a list, k: int): ''a list = if k = 0 then []
    else if k > 0 then raise SubSeqTooShort
    else []
  | css ([], l2, k) = if k = 0 then [] 
    else if k > 0 then raise SubSeqTooShort
    else []
  | css (x :: l1, y :: l2, k) = if k <= 0 then []
    else if lenSeq (x :: l1, 0) < k orelse lenSeq (y :: l2, 0) < k
    then raise SubSeqTooShort
    else if x = y andalso k > 0 then x :: css (l1, l2, k - 1)
    else css (l1, y :: l2, k)
    handle SubSeqTooShort => css (x :: l1, l2, k)

val [4, 7] = css ([3, 4, 5, 7], [4, 3, 5, 7], 2)