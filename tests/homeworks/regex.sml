
structure Regex: REGEX =
struct

datatype regex = One
               | Char of char
               | Times of regex * regex
               | Plus of regex * regex
               | Star of regex
               | Wild
               | Both of regex * regex

(* match: regex -> char list -> (char list -> bool) -> bool
   match r cs k = b
   ENSURES: b = true iff
    - cs = p@s
    - p in L(r)
    - k s = true
 *)

fun match (One: regex) (cs: char list) (k: char list -> bool) = k cs
  | match (Char c) cs k =
     (case cs
        of nil     => false
         | c'::cs' => c=c' andalso k cs')
  | match (Times(r1,r2)) cs k = 
      match r1 cs (fn cs' =>
      match r2 cs' k)
   | match (Plus(r1,r2)) cs k =
      match r1 cs k  orelse  match r2 cs k
   | match (Star r) cs k = k cs orelse
      match r cs (fn cs' =>
      cs <> cs' andalso match (Star r) cs' k)
   | match Wild cs k = 
      (case cs 
         of nil => false
        | c'::cs' => k cs')
   | match (Both (r1,r2)) cs k = 
      match r1 cs (fn cs' => match r2 cs 
        (fn cs'' => cs' = cs'' andalso k cs'))

val r1 = Star (Wild)  
val r2 = Times (Char #"f", Times (Char #"e", Char #"b"))
val r3 = Both (r1, r2) 
val true = match r1 ([#"D", #"E", #"D"]) (fn [] => true | _ => false)
val true = match r3 ([#"f", #"e", #"b"]) (fn [] => true | _ => false)

(* accept: regex -> string -> bool
   accept r s = b
   ENSURES: b = true iff s in L(r)
 *)
 
fun accept r s =
  match r (String.explode s) (fn [] => true | _ => false)

end (* structure Regex *)
