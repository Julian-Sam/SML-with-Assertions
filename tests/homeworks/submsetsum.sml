(* andrew Id: ____sjahmad____ *)

type certificate = int list

fun valid (n: int, []: int list): bool = false
  | valid (0, x :: l) = true
  | valid (n, x :: l) = valid (n - 1, l)

fun ith (0: int, x :: l: int list): int = x
  | ith (n, x :: l) = ith (n - 1, l)

fun notSimilarIndeces (n: int, []: certificate): bool = true
  | notSimilarIndeces (n, x :: l) = if n = x then false 
	else notSimilarIndeces (n, l)  

fun valid_cert ([]: certificate, l: int list) = true
  | valid_cert (x :: l1, l2) = valid (x, l2)
  	andalso notSimilarIndeces (x, l1) 
  	andalso valid_cert (l1, l2)

fun hindirect_add ([]: certificate, l2: int list, n: int): int = n
  | hindirect_add (x :: l1, l2, n) = let
  	val ithvalue = ith (x, l2)
  in
  	hindirect_add (l1, l2, n + ithvalue)
  end

fun indirect_add (l1: certificate, l2: int list): int = 
	  hindirect_add (l1, l2, 0)

val ~3 = indirect_add ([0, 1, 3], [1, 2, 1, ~6, 10])  

fun len ([]: int list, n: int): int = n
  | len (x :: l, a) = len (l, a + 1)

fun hsmSum (n: int, []: int list): bool = false
  | hsmSum (n, [x]) = if n = x then true else false 
  | hsmSum (n, x :: l) = let
    val (a, b) = (hsmSum (n, l), hsmSum (n - x, l))
  in
    a orelse b
  end

fun smSum (n: int, []: int list): bool = false
  | smSum (0, l) = true
  | smSum (n, l) = hsmSum (n, l)

fun certify (0: int, l1: int list, l2: int list, index: int): certificate = l2  
  | certify (n, [], l, a) = []
  | certify (n, x :: l, l2, a) = let
    val (c1, c2) = (certify (n - x, l, l2 @ [a], a + 1), certify (n, l, l2, a + 1))
  in
    if c1 = [] then c2
    else c1
  end

fun smSum_cert (0: int, l: int list): bool * certificate = (true, [])
  | smSum_cert (n, l) = if certify (n, l, [], 0) = [] then (false, [])
    else (true, certify (n, l, [], 0))

val l: bool * certificate = smSum_cert (6, [1, 2, 3, 4, 5]);

fun smSum_check (n: int, l: int list): bool = 
  let
    val (a,b) = smSum_cert (n, l) 
  in
    if a = true then n = indirect_add (b, l) else false
  end
