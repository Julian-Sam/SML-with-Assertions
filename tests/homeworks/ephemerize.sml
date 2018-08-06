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

  fun is_empty (a)
  	case (!a) of
  	  D.empty => true
  	| _   => false

  (*!
     REQUIRES: true
     ENSURES:  is_empty (result)
  !*)

  fun new (): 'a dict = ref D.empty

  (*!
  	 REQUIRES: true
     ENSURES:  let val (k, a) = e
               in (case (D.lookup (D_dict, k)) of SOME _ => true | _ => false) end
  !*)

  fun insert (D0: 'a dict, e: 'a entry): unit = let
    val D_dict = !(D0)
  in
    D0 := D.insert (D_dict, e)
  end

  (*!
  	 REQUIRES: true 
  	 ENSURES:  (case (D.lookup (D_dict, k)) of NONE => true | _ => false)
  !*)

  fun remove (D0: 'a dict, k: key): unit = let
    val D_dict = !(D0)
  in
    D0 := D.remove (D_dict, k)
  end

  (*!
  	 REQUIRES: true
     ENSURES: true
  !*)

  fun lookup (D0:'a dict, k: key): 'a option = let
    val D_dict = !(D0)
  in
    case (D.lookup (D_dict, k)) of
    SOME a => SOME a
    | NONE => NONE
  end


end (* functor Ephemerize *)
