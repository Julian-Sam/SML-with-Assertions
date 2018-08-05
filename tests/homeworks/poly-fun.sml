(* qatar Id: __sjahmad___ *)

functor PolyFun (U : UTIL) :> POLY =
struct
 type poly = int * (int -> int)

  val zero = (~1, (fn _ => 0)) (* REPLACE THIS LINE *) 

  (*
  fun | poly -> bool

  REQUIRES: valid (poly) 
  ENSURES: - true if polynomial is zero 
           - false otherwise
  *)

  (*
  fun isZero | poly -> bool

  REQUIRES:  valid (poly) 
  ENSURES: - true if polynomial is zero 
           - false otherwise
  *)

  fun isZero ((x, f): poly): bool = if x = ~1 
      then true else false

  (*
  fun scale | int * poly -> poly

  REQUIRES:  valid (poly) 
  ENSURES: - valid (poly)  
           - every coefficient is multiplied by m
  *)

  fun scale (n: int, (x, f): poly): poly = if n = 0 orelse isZero ((x, f))
      then zero else (x, fn m: int => n * f(m))

  (*
  fun add | poly (1) * poly  (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - valid (poly (3))
           - result = poly (1) + poly (2)     
  *)

  fun add ((x, f): poly, (y, g): poly): poly = if x > y 
      then (x, fn m: int => f(m) + g(m))
      else (y, fn m: int => f(m) + g(m))

  (*
  fun sub | poly (1) * poly (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2)) 
  ENSURES: - valid (poly (3)) 
           - poly(3) = poly (1) - poly(2)
  *)

  fun sub ((x, f): poly, (y, g): poly): poly = if x > y
      then (x, fn m: int => f(m) - g(m))
      else (y, fn m: int => f(m) - g(m))

  (*
  fun mult | poly (1) * poly (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - valid (poly (3))
           - poly(3) == poly(1) * poly (2)
  *)

  fun mult ((x, f): poly, (y, g): poly): poly = 
      (x + y, fn i => (Util.convolve (i, f, g)))

  (*
  fun lead | poly (1)  -> (int * int) (2)

  REQUIRES:  valid (poly (1))
  ENSURES: - the largest polynomial and its coefficient is returned in a tuple.
  *)

  fun lead ((x, f): poly): (int * int) = 
      if x = ~1 then (0, 0) else (x, f (x))

  (*
  fun differentiate_h | (int * int) list (1) * (int * int) list (2) -> (int * int) list (3)

  REQUIRES:  true
  ENSURES: - the returned int list is a list of the differentiated coefficients in the first list.
  *)

  fun differentiate ((x, f): poly): poly = 
      (x - 1, fn 0 => f(1) | m => (f (m + 1)) * (m + 1))

  (*
  fun pow | int (1) * int (2) -> int (3)

  REQUIRES: int (2) >= 0
  ENSURES: - pow (b, e) = b ^ e
  *)

  fun pow (b: int, 0: int): int = 1
    | pow (b, e) = b * pow (b, e - 1)

  (*
  fun eval_h | int * int * (int -> int) * int -> int

  REQUIRES:  true
  ENSURES: - accumulates the value of a * (x) ^ n for each element in the input int list)
  *)

  fun eval_h (~1: int, n: int, f: int -> int, acc: int): int = acc
    | eval_h (x, n, f, acc) = eval_h (x - 1, n, f, acc + (f(x) * pow (n, x)))

  (*
  fun eval | poly (1) * int (2) -> int (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - p (x) = a0 + a1 * x + a2 * x ^ 2 + ... + an * x ^ n
  *)

  fun eval ((x, f): poly) (n: int): int = eval_h (x, n, f, 0)

  (*
  fun fromList | int list (1) -> poly (2)

  REQUIRES:  true
  ENSURES: - valid (poly (2))
  *)

  fun fromList ([]: int list): poly = zero
    | fromList (x :: l) = let
      val l = Util.removeTrailing (0, x :: l)
      val last_index = length (l) - 1
    in
      (last_index, fn m => if m <= last_index then List.nth(l, m)
                                   else 0)
    end 

  (****************************************************************)

  (* sumString : string * string -> string
     sumString (s1, s2) = s
     ENSURES: s = s1 "+" s2
   *)
  fun sumStrings("":string, s2:string):string = s2
    | sumStrings(s1:string, "":string):string = s1
    | sumStrings(s1, s2) = s1 ^ " + " ^ s2

  (* toString : poly -> string
     toString p = s
     REQUIRES: valid p
     ENSURES: s is string rendering of polynomial p
   *)
  fun toString(p as (n, f)) =
    let

     (* coeffString : int -> string
        coeffString i = s
        ENSURES: s is a string rendering of the polynomial
                 of order i represented by f
      *)
     (* We could narrow n to something tight by binding
          val n:int = bound(p)
        but this isn't absolutely necessary since the
        function ignores 0 coefficients anyway.
      *)
     fun coeffString(i:int):string = 
        if i > n then ""
        else
         (case (i, f(i))
            of (i, 0) => coeffString(i+1)
             | (0, c) => sumStrings(Int.toString(c), coeffString(1))
             | (1, c) => sumStrings(Int.toString(c) ^ "*x", coeffString(2))
             | (i, c) => sumStrings(Int.toString(c) ^ "*x^" ^
                                        Int.toString(i), coeffString(i+1)))
      in
        if n = ~1 then "0" 
        else let
               val pstr:string = coeffString(0)
             in
               if pstr="" then "0" else pstr
             end
      end

  (* valid : poly -> bool
     valid p = b
     ENSURES: b iff p satisfies the functional representation invariant
   *)
  fun valid(n, f) =  (n + 1 >= 0) andalso f(n+1) = 0

  (****************************************************************)

end  (* Structure PolyFun *)
