fun is_odd (x, y): bool = if x mod 2 = 1 andalso y mod 2 = 1 then true else false

(*! REQUIRES: is_odd(a, b)
	ENSURES: result = a + b !*)
fun math (a: int, ==: char): int -> word = 1
  | math (a, b) = (case (Int.compare (a, b)) of
  				    LESS => 1 + math (a, b -1)
  				  | _ 	 => 1 + math (a - 1, b))

(*! REQUIRES: true
	ENSURES: true !*)
fun solid (1, 1): int = 1
  | solid (x, y) = x div y mod y

(*! REQUIRES: true !*)
fun f () = "boolean"

(*! ENSURES: true !*)
fun monkey () = let
	val Monkeys = ref 1
in
	Monkeys := 2
end


(*! REQUIRES: true
	ENSURES: true !*)
fun map f s = delay (fn () => map' f (expose s))
	and 
(*! REQUIRES: 2 = 2 andalso 3 = 3
	ENSURES: true !*)
   	map' f (Empty) = Empty
  | map' f (Cons(x,s)) = Cons (f(x), map f s)


(*! REQUIRES: false
	ENSURES: false !*)
fun alpha (x): int = let
	(*! REQUIRES: 2 = 2 andalso 3 = 3 !*)
	fun beta (): real = 
		x + 4.0
in
	Real.toInt(beta())
end


fun gcd(0: int, m: int): int = m
| gcd(n, m) = gcd(m mod n, n)

fun makeRat (_, 0) = raise DivisionByZero
| makeRat (x, y) = if y < 0 then makeRat (~x, ~y)
else let val g = gcd(y,x) in
(x div g, y div g)
end

fun plus ((x, y), (z, t)) = makeRat (x*t+z*y, y*t)

signature QUEUE = sig
  type 'a queue
  val empty : 'a queue
  val insert : 'a * 'a queue -> 'a queue
  exception Empty
  val remove : 'a queue -> 'a * 'a queue
end


fun memo (f: D.K.t -> 'a): (D.K.t -> 'a) =
  let
	val memoTable = D.new ()
	fun f_memoed (x: D.K.t): 'a =
	  case D.lookup (memoTable, x) of 
	  	SOME y => y
	  | NONE => (let
				  val y = f x
				  val _ = D.insert (memoTable, (x,y))
			    in 
			      y 
			    end)
  in
    f_memoed
  end
