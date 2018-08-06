(* andrew Id: _________________________________ *)

structure LookAndSay : LOOKANDSAY =
struct
  fun lookAndSay (eq: 'a * 'a -> bool) (s: 'a Seq.seq) :(int * 'a) Seq.seq =
      let fun lasHelp (s : 'a Seq.seq, x : 'a, acc : int) =
              case Seq.showl s
                of Seq.Nil => Seq.singleton (acc, x)
                 | Seq.Cons (y, ys) =>
                    if  eq (x,y)
                      then lasHelp (ys, x, acc+1)
                      else Seq.cons (acc, x) (lasHelp (ys, y, 1))
      in case Seq.showl s
           of Seq.Nil => Seq.empty ()
            | Seq.Cons (x, xs) => lasHelp (xs, x, 1)
      end
end (* structure LookAndSay *)
