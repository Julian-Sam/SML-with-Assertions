(* andrew Id: ___sjahmad___ *)

functor PolySparse (U : UTIL) :> POLY =
struct
  datatype poly = Zero
                | Sparse of (int*int) list

  val zero = Zero;

  (*
  fun rem_zero_coefficients | (int * int) list -> (int * int) list

  REQUIRES:  true 
  ENSURES: - result has no instances of (_, 0) elements
  *)

  fun rem_zero_coefficients ([]: (int * int) list): (int * int) list = []
    | rem_zero_coefficients ((x, y) :: l) = 
      if x = 0 then rem_zero_coefficients l
      else [(x, y)] @ rem_zero_coefficients l 

  (*
  fun isZero | poly -> bool

  REQUIRES:  valid (poly) 
  ENSURES: - true if polynomial is zero 
           - false otherwise
  *)

  fun isZero (Zero: poly): bool = true
    | isZero x = false

  (*
  fun scale_h | int * (int * int) list (1) * (int * int) list -> (int * int) list

  REQUIRES:  (int * int) list (1) from a valid (poly) 
  ENSURES: - (int * int) list represents scaled coefficients
  *)

  fun scale_h (n: int, []: (int * int) list, acc: (int * int) list)
      : (int * int) list = acc
    | scale_h (n, (x, y) :: l, acc) = scale_h (n, l, acc @ [(n * x, y)]) 

  (*
  fun scale | int * poly -> poly

  REQUIRES:  valid (poly) 
  ENSURES: - valid (poly)  
           - every coefficient is multiplied by m
  *)

  fun scale (n: int, Zero: poly): poly = Zero
    | scale (0, Sparse l) = Zero
    | scale (n, Sparse l) = Sparse (rem_zero_coefficients (scale_h (n, l, [])))

  (*
  fun add_h | (int * int) list * (int * int) list * (int * int) list -> (int * int) list

  REQUIRES: valid (poly) 
  ENSURES: - int list not empty  
           - int list represents scaled coefficients
  *)

  fun add_h ([]: (int * int) list, y: (int * int) list, acc: (int * int) list)
      : (int * int) list = acc @ y
    | add_h (x, [], acc) = acc @ x
    | add_h ((x, a) :: l1, (y, b) :: l2, acc) = if a < b 
      then add_h (l1, (y, b) :: l2, acc @ [(x, a)])
      else if b < a then add_h ((x, a) :: l1, l2, acc @ [(y, b)])
      else add_h (l1, l2, acc @ [(x + y, a)])

  (*
  fun zero_1 | (int * int) list -> bool

  REQUIRES:  true
  ENSURES: - there are only zeros in the input list.
  *)

  fun zero_1 ([]: (int * int) list): bool = true
    | zero_1 ((x, _) :: l) = if x = 0 then zero_1 l else false

  (*
  fun add | poly (1) * poly  (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - valid (poly (3))
           - result = poly (1) + poly (2)     
  *)

  fun add (Zero: poly, Zero: poly): poly = Zero
    | add (Zero, Sparse y) = Sparse y
    | add (Sparse x, Zero) = Sparse x
    | add (Sparse x, Sparse y) = let
      val iilist = add_h (x, y, [])
    in
      if not (zero_1 (iilist)) then Sparse (rem_zero_coefficients iilist) 
      else Zero
    end

  (*
  fun negate_elems | (int * int) list (1) -> int list

  REQUIRES: true 
  ENSURES: - all elements in int list (1) are negated.
             eg. (x, y) :: l = (~x, y) :: l
  *)

  fun negate_elems ([]: (int * int) list): (int * int) list = []
    | negate_elems ((x, y) :: l) = [(~x, y)] @ negate_elems (l) 

  (*
  fun sub_h | (int * int) list (1) * (int * int) list (2) * (int * int) list -> (int * int) list
  
  REQUIRES: int list (1) & (2) from valid (poly)
  ENSURES: - int list not empty  
           - int list represents scaled coefficients
  *)

  fun sub_h ([]: (int * int) list, y: (int * int) list, acc: (int * int) list)
      : (int * int) list = acc @ negate_elems(y)
    | sub_h (x, [], acc) = acc @ x
    | sub_h ((x, a) :: l1, (y, b) :: l2, acc) = if a < b 
      then sub_h (l1, (y, b) :: l2, acc @ [(x, a)])
      else if b < a then sub_h ((x, a) :: l1, l2, acc @ [(~y, b)])
      else sub_h (l1, l2, acc @ [(x - y, a)])

  (*
  fun sub | poly (1) * poly (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2)) 
  ENSURES: - valid (poly (3)) 
           - poly(3) = poly (1) - poly(2)
  *)

  fun sub (Zero: poly, Zero: poly): poly = Zero
    | sub (Zero, Sparse y) = Sparse (negate_elems (y))
    | sub (Sparse x, Zero) = Sparse x
    | sub (Sparse x, Sparse y) = let
      val iilist = sub_h (x, y, [])
    in
      if not (zero_1 (iilist)) then Sparse (rem_zero_coefficients iilist) 
      else Zero
    end

  (*
  fun create_long | (int * int) list (1) * (int * int) list (2) * int -> (int * int) list (3)

  REQUIRES:  true
  ENSURES: - appends orders of the polyomial with zero coefficients as well.
  *)

  fun create_long ([]: (int * int) list, acc: (int * int) list, index: int)
      : (int * int) list = acc
    | create_long ((x, y) :: l, acc, index) = if index < y then
      create_long ((x, y) :: l, acc @ [(0, index)], index + 1)
      else create_long (l, acc @ [(x, y)], index + 1) 

  (*
  fun append | (int * int) (1) * (int * int) list (2) * int -> (int * int) list

  REQUIRES:  ture
  ENSURES: - (int * int) (1) tuple is added to the right place in the (int * int) list
  *)

  fun append ((a, b): (int * int), []: (int * int) list, index: int)
      : (int * int) list = if b > index then append ((a, b), [(0, index)], 
      index + 1)
      else [(a, b)]
    | append ((x, a), (y, b) :: acc, index) = if a > b 
      then [(y, b)] @ append ((x, a), acc, index + 1)
      else acc @ [(x + y, a)]

  (*
  fun mult_append | (int * int) (1) * (int * int) list (2) * (int * int) list -> (int * int) list

  REQUIRES:  true
  ENSURES: - multiplies element (int * int) (1) with all elements in the second input (int * int) list
  *)

  fun mult_append (_: (int * int), []: (int * int) list, acc: (int * int) list)
      : (int * int) list = acc
    | mult_append ((x, a), (y, b) :: l, acc) = let
      val acc = append ((x * y, a + b), acc, 0)
    in
      mult_append ((x, a), l, acc)
    end

  (*
  fun mult_h | (int * int) list (1) * int list (2) * int list -> int list

  REQUIRES:  true
  ENSURES: - multiplies all elements in int line (1) to 
             all elements in int list (2) and returns an accumulated list.
  *)

  fun mult_h ([]: (int * int) list, y: (int * int) list, acc: (int * int) list)
      : (int * int) list = acc
    | mult_h ((x, a) :: l, y, acc) = let
      val acc = mult_append ((x, a), y, acc)
    in
      mult_h (l, y, acc)
    end 

  (*
  fun mult | poly (1) * poly (2) -> poly (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - valid (poly (3))
           - poly(3) == poly(1) * poly (2)
  *)

  fun mult (Zero: poly, Zero: poly): poly = Zero
    | mult (Zero, Sparse _) = Zero
    | mult (Sparse _, Zero) = Zero
    | mult (Sparse x, Sparse y) = 
      Sparse (rem_zero_coefficients (mult_h (create_long(x, [], 0), 
      create_long(y, [], 0), [])))

  (*
  fun lead | poly (1)  -> (int * int) (2)

  REQUIRES:  valid (poly (1))
  ENSURES: - the largest polynomial and its coefficient is returned in a tuple.
  *)

  fun lead (Zero: poly): int * int = (0, 0)
    | lead (Sparse [(x, y)]) = (y, x)
    | lead (Sparse (x :: l)) = lead (Sparse (l))

  (*
  fun differentiate_h | (int * int) list (1) * (int * int) list (2) -> (int * int) list (3)

  REQUIRES:  true
  ENSURES: - the returned int list is a list of the differentiated coefficients in the first list.
  *)

  fun differentiate_h ([]: (int * int) list, acc: (int * int) list)
      : (int * int) list = acc
    | differentiate_h ((x, y) :: l, acc) = if y = 0 
      then differentiate_h (l, acc)
      else differentiate_h (l, acc @ [(y * x, y - 1)]) 

  (*
  fun differentiate | poly (1) -> poly (2)

  REQUIRES:  valid (poly (1))
  ENSURES: - valid (poly (2))
           - poly (2) is the differentiated congruent of poly (1)
  *)

  fun differentiate (Zero: poly): poly = Zero
    | differentiate (Sparse [(_ , 0)]) = Zero
    | differentiate (Sparse x) = 
      Sparse (rem_zero_coefficients (differentiate_h (x, [])))

  (*
  fun pow | int (1) * int (2) -> int (3)

  REQUIRES: int (2) >= 0
  ENSURES: - pow (b, e) = b ^ e
  *)

  fun pow (n: int, 0: int) = 1
    | pow (n, e) = n * pow (n, e - 1)

  (*
  fun eval_h | (int * int) list (1) * int -> int

  REQUIRES:  true
  ENSURES: - accumulates the value of a * (x) ^ n for each element in the input int list)
  *)

  fun eval_h ([]: (int * int) list) (n: int): int = 0
    | eval_h ((x, y) :: l) (n) = x * (pow (n, y)) + eval_h l n  

  (*
  fun eval | poly (1) * int (2) -> int (3)

  REQUIRES:  valid (poly (1)) & valid (poly (2))
  ENSURES: - p (x) = a0 + a1 * x + a2 * x ^ 2 + ... + an * x ^ n
  *)

  fun eval (Zero: poly) (n: int): int = 0
    | eval (Sparse l) (n) = eval_h l n

  (*
  fun fromList_h | int list (1) * (int * int) list * int -> poly (2)

  REQUIRES:  true
  ENSURES: - returns (int * int) list from int list with each coefficient appended
             with its order.
  *)

  fun fromList_h ([]: int list, acc: (int * int) list, index: int)
    : (int * int) list = acc
    | fromList_h (x :: l, acc, index) = fromList_h (l, acc @ [(x, index)], 
      index + 1)

  (*
  fun fromList | int list (1) -> poly (2)

  REQUIRES:  true
  ENSURES: - valid (poly (2))
  *)

  fun fromList ([]: int list): poly = Zero
    | fromList (l) = if Util.removeTrailing (0, l) <> [] then 
      Sparse (rem_zero_coefficients (
      fromList_h (Util.removeTrailing (0, l), [], 0))) else Zero


  (****************************************************************)
  (* sumStrings : string * string -> string
     sumStrings (s1, s2) = s
     ENSURES: s = s1 ^ " + " ^ s2, unless s1 or s2 is empty in which
              case s is the other string (possibly "")
   *)
   
   fun sumStrings("":string, s2:string):string = s2
     | sumStrings(s1:string, "":string):string = s1
     | sumStrings(s1, s2) = s1 ^ " + " ^ s2

    (* toString : poly -> string
       toString p = s
       REQUIRES: valid p
       ENSURES: s is a string representation of p(x)
    *)
    fun toString Zero = "0"
      | toString (Sparse([])) = ""
      | toString (Sparse((c,0)::clist)) = sumStrings(Int.toString(c), toString(Sparse(clist)))
      | toString (Sparse((c,1)::clist)) = sumStrings(Int.toString(c) ^ "*x",toString(Sparse(clist)))
      | toString (Sparse((c,p)::clist)) = sumStrings(Int.toString(c) ^ "*x^" ^ Int.toString(p),toString(Sparse(clist)))
    
  
  (****************************************************************)
  (* valid : poly -> bool
     valid p = b
     ENSURES: b iff p satisfies the sparse representation invariants
   *)
  fun valid Zero = true
    | valid (Sparse []) = false
    | valid (Sparse clist) =
       case (foldl (fn ((c,e1),(b,e2)) =>
                       ((b andalso not(c = 0) andalso e1>e2), e1))
                   (true,~1) clist) 
         of (b,_) => b
         
  (****************************************************************)

end  (* structure PolySparse *)
