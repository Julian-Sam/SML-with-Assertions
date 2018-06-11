fun sum (a : int, b : int): int = a + b
  | sum (1, 2) = 12
  | sum (a, 3) = sum (12, 24);

fun sum f 0 = 0
  | sum f n = (f n) + sum (f (n-1));

fun p 1 = 1
  | p n = sum (fn k => (p k) * (p (n-k)) (n-1));

(*fun f () =
  let
    fun a () = b ()
    and b () = a ()
  in
    a ()
  end*)