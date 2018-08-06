(* andrew Id: __sjahmad__ *)

fun choose (n, k) = 


(*!
   REQUIRES: 0 <= k andalso k <= n
   ENSURES: result >= 0
!*) 
fun pascal (n: int, k: int): int = if k = 0 then 1 
								   else if n = k then 1
    else pascal (n-1, k-1) + pascal (n-1, k);

val 6 = pascal (4,2);
val 8 = pascal (8,1);
val 1 = pascal (16,0);
val 1 = pascal(0,0);
val 1 = pascal(5,0);

fun pow (b, 0) = 1
  | pow (b, e) = if e = 1 then b
				 else b * pow (b, e - 1)

(*!
   REQUIRES: n >= 0
   ENSURES: result = pow(2, n)
!*)

fun expRecurse (n: int, b: int): int = if n = b then 1
  else pascal (n, n - b) + expRecurse (n, b + 1)

fun exp (n: int): int = expRecurse (n, 0);

val 1 = exp(0);
val 2 = exp(1);
val 256 = exp(8);
val 4 = exp(2);