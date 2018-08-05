(* andrew Id: __sjahmad__ *)

(* pascal: int * int -> int
 * REQUIRES: 0 <= k <= n
 * ENSURES:  That the result evaluates to n choose k.
 *) 
fun pascal (n: int, 0: int): int = 1
  | pascal (n, k) = if n = k then 1
    else pascal (n-1, k-1) + pascal (n-1, k);

val 6 = pascal (4,2);
val 8 = pascal (8,1);
val 1 = pascal (16,0);
val 1 = pascal(0,0);
val 1 = pascal(5,0);

(* exp: int -> int
 * REQUIRES: n >= 0.
 * ENSURES:  That the number n evaluates to 2^n.
 *)

fun expRecurse (n: int, b: int): int = if n = b then 1
  else pascal (n, n - b) + expRecurse (n, b + 1)

fun exp (n: int): int = expRecurse (n, 0);

val 1 = exp(0);
val 2 = exp(1);
val 256 = exp(8);
val 4 = exp(2);