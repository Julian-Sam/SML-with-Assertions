structure IStream :> ISTREAM =
struct
  datatype ('a,'b) stream = Stream of 'a -> ('a,'b) front
  and      ('a,'b) front  = End
                          | Gen of 'b * ('a,'b) stream

 (* --------------------------------------------------------------
    fun delay: ('a -> ('a,'b) front) -> ('a,'b) stream

    REQUIRES: - Be a valid function definition
    ENSURES:  - The delayed computation be mapped to a stream.
    -------------------------------------------------------------- *)

  fun delay (f: ('a -> ('a,'b) front)): ('a,'b) stream = Stream f

 (* --------------------------------------------------------------
    fun expose: ('a,'b) stream -> 'a -> ('a,'b) front

    REQUIRES: - true
    ENSURES:  - Ensures that this function returns the 
                                   next exposed part of the stream
    -------------------------------------------------------------- *)

  fun expose (Stream s: ('a,'b) stream) (x: 'a): ('a,'b) front = s (x)

 (* ------------------------------------------------------------------
    fun map:  ('a -> 'b -> 'c) -> ('a,'b) stream -> ('a,'c) stream
    and map': ('a -> 'b -> 'c) -> 'a -> ('a,'b) front -> ('a,'c) front

    REQUIRES: - Valid mapping function
    ENSURES:  - Ensures all 'b elements are lazily mapped to 'c 
                elements using the 'a arguments passed in by the user.
    ------------------------------------------------------------------ *)

  fun map (f : ('a -> 'b -> 'c)) (S: ('a,'b) stream): ('a,'c) stream =
      delay ((fn a => map' (f) (a) (expose S a)))

  and map' (f: ('a -> 'b -> 'c)) (a: 'a) (End: ('a,'b) front): ('a,'c) front = End
    | map' (f) (a) (Gen (b, s)) = Gen (f (a) (b), map f (s))

 (* ----------------------------------------------------------------------
    fun filter:  ('a * 'b -> bool) -> ('a,'b) stream -> ('a,'b) stream
    and filter': ('a * 'b -> bool) -> 'a -> ('a,'b) front -> ('a,'b) front

    REQUIRES: - Valid predicate function.
    ENSURES:  - Ensures all 'b elements that with 'a argments not 
                     satisfying the predicate are removed from the stream.
    ---------------------------------------------------------------------- *)

  fun filter (p: ('a * 'b -> bool)) (S: ('a,'b) stream): ('a,'b) stream =
      delay ((fn a => filter' (p) (a) (expose S a)))

  and filter' (p: ('a * 'b -> bool)) (a: 'a) (End: ('a,'b) front): ('a,'b) front = End
    | filter' (p) (a) (Gen (b, s)) = case (p (a, b)) of
                                     true => Gen (b, filter (p) (s))
                                  | false => (filter' (p) (a) (expose s a))

 (* -----------------------------------------------------------------
    fun take:  ('a,'b) stream -> 'a list -> 'b list

    REQUIRES: - true
    ENSURES:  - Returns the 'b values in the stream that are given by
                exposing the stream to every 'a value in the list.

              - There are atmost len ('a list) elements in 'b list
    ----------------------------------------------------------------- *)

  fun take (S: ('a,'b) stream) ([]: 'a list): 'b list = []
    | take (Stream f) (x :: L) =   
                  case (f (x)) of
                          End => []
                  | Gen (beta, S) => beta :: take (S) (L)

 (* ------------------------------------------------------------------
    fun drop:  ('a,'b) stream -> 'a list -> 'b list

    REQUIRES: - true
    ENSURES:  - Returns the stream that we obtain by exposing 
                the input stream to all elements in the input 'a list.
    ------------------------------------------------------------------ *)

  fun drop (S: ('a,'b) stream) ([]: 'a list): ('a,'b) stream = S
    | drop (Stream s) (x :: L) =
                  case (s (x)) of
                    End => delay ((fn x => End))
                    | Gen (b, S) => drop (S) (L)

 (* -----------------------------------------------------------------------------
    fun fromFun:  ('a -> 'b) -> ('a,'b) stream

    REQUIRES: - valid mapping function.
    ENSURES:  - Stream formed consists of 'b elements that are reuslts
                of applying the function on arguments that they were exposed with
    ----------------------------------------------------------------------------- *)

  fun fromFun (f: ('a -> 'b)): ('a,'b) stream =
    delay (fn a => Gen (f (a), fromFun f))

  (* Bonus *)

 (* -----------------------------------------------------------------------------
    fun compose:  ('a,'b) stream -> ('b -> 'c) -> ('a,'c) stream
    and compose': ('b -> 'c) -> ('a,'b) front -> ('a,'c) front

    REQUIRES: - valid mapping function.
    ENSURES:  - Takes the 'b elements in the stream and maps them to 'c function 
                using the mapping function irrespective of the argument passed in
    ----------------------------------------------------------------------------- *) 

  fun compose (s: ('a,'b) stream) (f: ('b -> 'c)): ('a,'c) stream =
    delay ((fn a => compose' (f) (expose (s) (a))))
    
  and compose' (f: ('b -> 'c)) (End: ('a,'b) front): ('a,'c) front =End
    | compose' (f) (Gen (b, s)) = Gen (f (b), compose (s) (f))

 (* -----------------------------------------------------------------------------
    fun iterate:  ('a -> 'a) -> ('a) -> ('a,'b) stream

    REQUIRES: - valid mapping function.
    ENSURES:  - Returns a new stream that for every ith element it performs the 
                    mapping function i times.
    ----------------------------------------------------------------------------- *) 

  fun iterate (f: ('a -> 'a)) (x0: 'a): ('a,'a) stream =
      delay (fn a => Gen (x0, iterate (f) (f (x0))))

 (* toString (a_ts,b_ts) S l ==> s
    ENSURES: s is a string representation of exposing stream S with
             list of values l
  *)
 fun toString (_: 'a -> string, _: 'b -> string) ([]: 'a list)
	      (_: ('a,'b) stream): string = "."
   | toString (a_ts, b_ts) (a::l) S =
       a_ts a ^ " > " ^ toString' (a_ts, b_ts) l (expose S a)
 and toString'(a_ts, b_ts) l (Gen (b,S)) =
       b_ts b ^ "; " ^ toString (a_ts,b_ts) l S
   | toString' _ _ End = "!"

 (* eq b_ts (S1,S2) l ==> b
    ENSURES: b == true iff exposing both streams S1 and S2 with the
	     same list of values l yields the same values in the same
             order.
  *)
 fun eq (_: 'b * 'b -> bool) ([]: 'a list)
	(_: ('a,'b) stream, _: ('a,'b) stream): bool = true
   | eq b_eq (a::l) (S1,S2) = eq' b_eq l (expose S1 a, expose S2 a)
 and eq' b_eq l (Gen (b1,S1), Gen (b2,S2)) =
          b_eq (b1,b2) andalso eq b_eq l (S1,S2)
   | eq' _ _ (End, End) = true
   | eq' _ _ _ = false
end (* structure IStream *)
