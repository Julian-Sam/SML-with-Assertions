(* andrew Id: _____sjahmad______ *)

functor PolyList (U : UTIL) :> POLY =
struct
  datatype poly = Zero | Coeff of int list

  val zero = Zero   (* REPLACE THIS LINE *) 

  (*
  fun isZero | poly -> bool

  REQUIRES:  valid (poly) 
  ENSURES: - true if polynomial is zero 
           - false otherwise
  *)

  fun isZero (Zero: poly): bool = true
    | isZero x = false

  (*
  fun scale_h | int * int list (1) * int list -> int list

  REQUIRES:  int list (1) from a valid (poly) 
  ENSURES: - int list not empty  
           - int list represents scaled coefficients
  *)

  fun scale_h (m: int, []: int list, acc: int list): int list = acc
    | scale_h (m, x :: l, acc) = scale_h (m, l, acc @ [m * x])

  (*
  fun scale | int * poly -> poly

  REQUIRES:  valid (poly) 
  ENSURES: - valid (poly)  
           - every coefficient is multiplied by m
  *)

  fun scale (m: int, Zero: poly): poly = Zero
    | scale (0, Coeff x) = Zero
    | scale (m, Coeff l) = Coeff (scale_h (m, l, []))

  (*
  fun not_zero | int list -> bool

  REQUIRES:  int list from a valid (poly) 
  ENSURES: - there are no zeros in the input list.
  *)

  fun not_zero ([]: int list): bool = false
    | not_zero (_) = true

  (*
  fun add_h | int list * int list * int list -> int list

  REQUIRES: valid (poly) 
  ENSURES: - int list not empty  
           - int list represents scaled coefficients
  *)

  fun add_h ([]: int list, y: int list, acc: int list): int list = acc @ y
    | add_h (x, [], acc) = acc @ x
    | add_h (x :: l1, y :: l2, acc) = add_h (l1, l2, acc @ [x + y])

  (*
  fun add | poly (1) * poly  (2) -> poly (3)

  REQUIRES: valid (poly (1)) & valid (poly (2))
  ENSURES: - valid (poly (3))
           - result = poly (1) + poly (2) 
           
  *)

  fun add (Zero: poly, Zero: poly): poly = Zero
    | add (Zero, Coeff y) = Coeff y
    | add (Coeff x, Zero) = Coeff x
    | add (Coeff x, Coeff y) = let
      val ilist = add_h (x, y, [])
    in
      if not_zero (Util.removeTrailing(0, ilist)) then Coeff (ilist) else Zero
    end

  (*
  fun negate_elems | int list (1) -> int list

  REQUIRES: true 
  ENSURES: - all elements in int list (1) are negated.
  *)

  fun negate_elems ([]: int list): int list = []
    | negate_elems (x :: l) = [~x] @ negate_elems (l) 

  (*
  fun sub_h | int list (1) * int list (2) * int list -> int list

  REQUIRES: int list (1) & (2) from valid (poly) 
  ENSURES: - int list not empty  
           - int list represents scaled coefficients
  *)

  fun sub_h ([]: int list, y: int list, acc: int list): int list = 
      acc @ negate_elems(y)
    | sub_h (x, [], acc) = acc @ x
    | sub_h (x :: l1, y :: l2, acc) = sub_h (l1, l2, acc @ [x - y])

  (*
  fun sub | poly (1) * poly (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2)) 
  ENSURES: - valid (poly (3)) 
           - poly(3) = poly (1) - poly(2)
  *)

  fun sub (Zero: poly, Zero: poly): poly = Zero
    | sub (Zero, Coeff y) = Coeff (negate_elems (y))
    | sub (Coeff x, Zero) = Coeff x
    | sub (Coeff x, Coeff y) = let
      val ilist = sub_h (x, y, [])
    in
      if not_zero (Util.removeTrailing(0, ilist)) then Coeff (ilist) else Zero
    end

  (*
  fun get_Index | int (1) * int list (2) -> int

  REQUIRES:  true
  ENSURES: - int list (2) [result] is equal to int (1)
  *)

  fun get_Index (m: int, []: int list, acc: int) = raise Fail "Element not in INT LIST"
    | get_Index (m, x :: l, acc) = if m = x then acc
      else get_Index (m, l, acc + 1)

  (*
  fun append_acc | int (1) * int list (2) * int * int -> int list

  REQUIRES:  true
  ENSURES: - Makes sure that 
  *)

  fun append_acc (m: int, []: int list, index1: int, index2: int): int list =
      if index1 > index2 then append_acc (m, [0], index1, index2 + 1)
      else [m]
    | append_acc (m, y :: acc, index1, index2) = if index1 = index2
      then [m + y] @ acc
      else [y] @ append_acc (m, acc, index1, index2 + 1)

  (*
  fun multiply_x | int (1) * int list (2) * int list * int * int -> int list

  REQUIRES:  true
  ENSURES: - multiplies int (1) to all elements in int list (2) and returns an accumulated list.
  *)

  fun multiply_x (x: int, []: int list, acc: int list, x_index: int, 
      y_index: int): int list = acc
    | multiply_x (x, y :: l, acc, x_index, y_index) = let
      val acc = append_acc (x * y, acc, x_index + y_index, 0)
    in
      multiply_x (x, l, acc, x_index, y_index + 1)
    end

  (*
  fun mult_h | int list (1) * int list (2) * int list -> int list

  REQUIRES:  true
  ENSURES: - multiplies all elements in int line (1) to 
             all elements in int list (2) and returns an accumulated list.
  *)

  fun mult_h ([]: int list, y: int list, acc: int list, x_index: int)
      : int list = acc 
    | mult_h (x :: l, y, acc, x_index) = let
      val acc = multiply_x (x, y, acc, x_index, 0)
    in
      mult_h (l, y, acc, x_index + 1)
    end

  (*
  fun mult | poly (1) * poly (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - valid (poly (3))
           - poly(3) == poly(1) * poly (2)
  *)

  fun mult (Zero: poly, Zero: poly): poly = Zero
    | mult (Zero, Coeff y) = Zero
    | mult (Coeff x, Zero) = Zero
    | mult (Coeff x, Coeff y) = Coeff (mult_h (x, y, [], 0))

  (*
  fun lead_h | int (1) * int list (2) -> (int * int)

  REQUIRES:  true
  ENSURES: - the largest polynomial and its coefficient is returned in a tuple.
  *)

  fun lead_h (index: int, x :: nil: int list): (int * int) = (index, x)
    | lead_h (index, x :: l) = lead_h (index + 1, l)

  (*
  fun lead | poly (1)  -> (int * int) (2)

  REQUIRES:  valid (poly (1))
  ENSURES: - the largest polynomial and its coefficient is returned in a tuple.
  *)

  fun lead (Zero: poly): int * int = (0, 0)
    | lead (Coeff x) = lead_h (0, Util.removeTrailing(0, x))

  (*
  fun differentiate_h | int list (1) * int list (2) * int -> int list (3)

  REQUIRES:  true
  ENSURES: - the returned int list is a list of the differentiated coefficients in the first list.
  *)

  fun differentiate_h ([]: int list, acc: int list, index: int): int list = acc
    | differentiate_h (x :: l, acc, index) = if index = 0 then 
      differentiate_h (l, acc, index + 1) 
      else differentiate_h (l, acc @ [index * x], index + 1)

  (*
  fun differentiate | poly (1) -> poly (2)

  REQUIRES:  valid (poly (1))
  ENSURES: - valid (poly (2))
           - poly (2) is the differentiated congruent of poly (1)
  *)

  fun differentiate (Zero: poly): poly = Zero
    | differentiate (Coeff ([x])) = Zero
    | differentiate (Coeff x) = Coeff (differentiate_h (x, [], 0))

  (*
  fun pow | int (1) * int (2) -> int (3)

  REQUIRES: int (2) >= 0
  ENSURES: - pow (b, e) = b ^ e
  *)

  fun pow (n: int, 0: int): int = 1
    | pow (n, e) = n * pow (n, e - 1)

  (*
  fun eval_h | int list (1) * int * int * int (4) -> int

  REQUIRES:  int (4) >= 0 & int (4) < len (int list (1))
  ENSURES: - accumulates the value of a * (x) ^ n for each element in the input int list)
  *)

  fun eval_h ([]: int list, n: int, acc: int, index: int): int = acc
    | eval_h (x :: l, n, acc, index) = 
      eval_h (l, n, acc + (x * pow(n, index)), index + 1)

  (*
  fun eval | poly (1) * int (2) -> int (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - p (x) = a0 + a1 * x + a2 * x ^ 2 + ... + an * x ^ n
  *)

  fun eval (Zero: poly) (n: int): int = 0
    | eval (Coeff x) (n) = eval_h (x, n, 0, 0)

  (*
  fun zero_List | int list (1) -> bool (2)

  REQUIRES:  true
  ENSURES: - returns true if list has all zeros or is empty
           - returns false otherwise
  *)

  fun zero_List ([]: int list): bool = true
    | zero_List (x :: l) = if x = 0 then zero_List (l)
      else false 

  (*
  fun fromList | int list (1) -> poly (2)

  REQUIRES:  true
  ENSURES: - valid (poly (2))
  *)

  fun fromList ([]: int list): poly = Zero
    | fromList (x) = if Util.removeTrailing (0, x) <> [] then 
      Coeff (Util.removeTrailing (0, x)) else Zero

  (****************************************************************)

  (* sumString : string * string -> string
     sumString (s1, s2) = s
     ENSURES: s = s1 "+" s2
   *)
  fun sumStrings("":string, s2:string):string = s2
    | sumStrings(s1:string, "":string):string = s1
    | sumStrings(s1, s2) = s1 ^ " + " ^ s2

  (* coefString : int * int list -> string
     coeffString (i, l) = s
     ENSURES: s is a string rendering of the polynomial
              of order i represented by l
   *)
  fun coeffString(_:int, []:int list): string = ""
    | coeffString(i, 0::clist) = coeffString(i+1, clist)
    | coeffString(0, c::clist) =
       sumStrings(Int.toString(c), coeffString(1, clist))
    | coeffString(1, c::clist) =
       sumStrings(Int.toString(c) ^ "*x", coeffString(2, clist))
    | coeffString(i, c::clist) =
       sumStrings(Int.toString(c) ^ "*x^" ^ Int.toString(i),
                  coeffString(i+1, clist))

  (* toString : poly -> string
     toString p = s
     REQUIRES: valid p
     ENSURES: s is string rendering of polynomial p
   *)
  fun toString(Zero) = "0"
    | toString(Coeff(clist)) = coeffString(0, clist)

  (* valid : poly -> bool
     valid p = b
     ENSURES: b iff p satisfies the list representation invariant
   *)
  fun valid(Zero) = true
    | valid(Coeff[]) = false
    | valid(Coeff(clist)) = 
       not (List.nth(clist, List.length(clist) - 1) = 0)
       
  (****************************************************************)

(* Test Cases *)

(* (1) - isZero *)

val true = isZero (Zero)
val false = isZero (Coeff [1])

(* (2) - scale *)

val Coeff [23] = scale (23, Coeff [1])

(* (3) - add *)

val Coeff [1] = add (Coeff [1], Zero)
val Coeff [2, 3] = add (Coeff [2, 2], Coeff [0, 1])

(* (3) - sub *)

val Coeff [~1] = sub (Zero, Coeff [1])
val Coeff [4, 1] = sub (Coeff [4, 2], Coeff [0, 1])

(* (5) - mult *)

val Coeff [0, 1] = mult (Coeff [1], Coeff [0, 1])

(* (6) - lead *)

val (1, 1) = lead (Coeff [2, 1])

(* (7) - differentiate *)

val Coeff [1, 4, 9] = differentiate (Coeff [0, 1, 2, 3])

(* (8) - eval *)

val 5 = eval (Coeff [0, 1])(5)

(* (9) - fromList *)

val Zero = fromList ([0, 0, 0, 0,0,0])
val Coeff [0, 1, 2, 3] =  fromList ([0, 1, 2, 3])

end  (* structure PolyList *)

