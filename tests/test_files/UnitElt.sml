structure UnitElt : ELEMENT =
struct
  type t = unit

  exception NYI

  val default = ()
  val equal = true
  fun compare (x,y) = raise NYI
  fun hash x = raise NYI
  fun toString (_ : t) = "()"
end
