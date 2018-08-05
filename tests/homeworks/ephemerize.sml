functor Ephemerize (D: DICT) :> EPHDICT where K = D.K =
struct
  structure K = D.K
  type key = K.t
  type 'a entry = key * 'a
  type 'a dict = 'a D.dict ref

  val entry_toString = D.entry_toString
  val entry_eq       = D.entry_eq

  fun toString (f: ('a -> string)) (d: 'a dict): string = let
    val dict1 = !d
  in
    D.toString (f) (dict1)
  end

   (* use for testing *)
  fun eq (f: ('a * 'a -> bool)) (d_0: 'a dict, d_1: 'a dict): bool =
      D.eq (f) (!d_0, !d_1) (* use for testing *)

  (* ------------------------------------------------------
     fun new: unit -> 'a dict
     REQUIRES: true
     ENSURES:  result is a reference to an empty dictionary
     ------------------------------------------------------ *)

  fun new (): 'a dict = ref D.empty

  (* --------------------------------------------------
     fun insert: 'a dict (1) * 'a entry (2) -> unit
     REQUIRES: valid reference to a dictionary
     ENSURES:  Let 'a entry (2) = (key, 'a (3))
               lookup ('a dict (1), key) == SOME 'a (3)
     -------------------------------------------------- *)

  fun insert (D0: 'a dict, e: 'a entry): unit = let
    val D_dict = !(D0)
  in
    D0 := D.insert (D_dict, e)
  end

  (* -----------------------------------------------
     fun remove: 'a dict (1) * key (2) -> unit
     REQUIRES: valid reference to a dictionary
     ENSURES:  lookup ('a dict (1), key (2)) == NONE
     ----------------------------------------------- *)

  fun remove (D0: 'a dict, k: key): unit = let
    val D_dict = !(D0)
  in
    D0 := D.remove (D_dict, k)
  end

  (* --------------------------------------------------------
     fun lookup: 'a dict (1) * key (2) -> 'a option
     REQUIRES: valid reference to a dictionary
     ENSURES:  NONE if key (2) not in 'a dict (1)
               SOME 'a if 'a entry - (key, 'a) in 'a dict (1)
     -------------------------------------------------------- *)

  fun lookup (D0:'a dict, k: key): 'a option = let
    val D_dict = !(D0)
  in
    case (D.lookup (D_dict, k)) of
    SOME a => SOME a
    | NONE => NONE
  end


end (* functor Ephemerize *)
