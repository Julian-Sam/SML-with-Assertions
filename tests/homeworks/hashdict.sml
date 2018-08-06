functor HashDict (structure Key: KEY
                        val nBuckets: int) :> EPHDICT where K = Key =
struct
  structure K = Key
  type key = K.t
  type 'a entry = key * 'a

  (* Representation invariant: duplicate keys are allowed in each
     bucket, but only the first is returned on lookup; remove scrubs
     the bucket *) 
  type 'a bucket = 'a entry list
  type 'a dict = 'a bucket ref Seq.seq ref

  fun entry_toString (ts: 'a -> string) (k: key, x: 'a): string =
    K.toString k ^ "->" ^ ts x

  fun entry_eq (a_eq: 'a * 'a -> bool) ((k1,x1): 'a entry, (k2,x2): 'a entry): bool =
      K.eq (k1,k2) andalso a_eq (x1,x2)


  fun toString _ = raise Fail "Unimplemented" (* use for testing *)
  fun eq       _ = raise Fail "Unimplemented" (* use for testing *)

  fun Hash (k: key): int = K.toInt (k) mod nBuckets

  (* ------------------------------------------------------
     fun new: unit -> 'a dict
     REQUIRES: true
     ENSURES:  result is a dictionary of size nBuckets each
               referring to empty entry lists.
     ------------------------------------------------------ *)

  fun new (): 'a dict = ref (Seq.tabulate (fn x => ref []) (nBuckets))

  (* --------------------------------------------------
     fun insert: 'a dict (1) * 'a entry (2) -> unit
     REQUIRES: valid reference to a dictionary
     ENSURES:  Let 'a entry (2) = (key, 'a (3))
               lookup ('a dict (1), key) == SOME 'a (3)
     -------------------------------------------------- *)

  fun insert (D0: 'a dict, (k, a): 'a entry): unit = let
    val D_dict = !D0
    val L_Ref = Seq.nth (D_dict) (Hash (k))
    val B_bucket = !L_Ref
  in
    L_Ref := ((k, a) :: B_bucket)
  end

  (* ----------------------------------------------------------------
     fun remove_Key: 'a entry list (1) * key (2) -> 'a entry list (3)
     REQUIRES: true
     ENSURES:  Let 'a entry list (3) = (key, 'a (4))
               lookup (dict, key) == NONE
     ---------------------------------------------------------------- *)

  fun remove_Key ([]: 'a entry list, k: key): 'a entry list = []
    | remove_Key ((k1, a) :: e, k) = 
      if K.eq (k1, k) then e
      else (k1, a) :: remove_Key (e, k)

  (* -----------------------------------------------
     fun remove: 'a dict (1) * key (2) -> unit
     REQUIRES: valid reference to a dictionary
     ENSURES:  lookup ('a dict (1), key (2)) == NONE
     ----------------------------------------------- *)

  fun remove (D0: 'a dict, k: key): unit = let
    val D_dict = !D0
    val L_Ref = Seq.nth (D_dict) (Hash (k))
    val B_bucket = !L_Ref
  in
    L_Ref := (remove_Key (B_bucket, k))
  end

  (* -------------------------------------------------------------
     fun find_Key: 'a entry list (1) * key (2) -> 'a option
     REQUIRES: true
     ENSURES:  If there exists an entry in 'a entry list such that
               its key is equal to key (2) then it returns SOME of 
               it corresponding 'a value else return NONE.
     ------------------------------------------------------------- *)

  fun find_Key ([]: 'a entry list, k: key): 'a option = NONE
    | find_Key ((k1, a) :: e, k) = 
      if K.eq (k1, k) then SOME (a)
      else find_Key (e, k)

  (* --------------------------------------------------------
     fun lookup: 'a dict (1) * key (2) -> 'a option
     REQUIRES: valid reference to a dictionary
     ENSURES:  NONE if key (2) not in 'a dict (1)
               SOME 'a if 'a entry - (key, 'a) in 'a dict (1)
     -------------------------------------------------------- *)

  fun lookup (D0: 'a dict, k: key): 'a option = let
    val D_dict = !D0
    val B_bucket = !(Seq.nth (D_dict) (Hash (k)))
  in
    find_Key (B_bucket, k)
  end

end (* structure HashDict *)
