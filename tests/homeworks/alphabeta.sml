(* andrew Id: ___sjahmad___ *)

functor MakeAlphaBeta (structure G : GAME
                       val search_depth : int)
        : ALPHABETA where type Game.move = G.move
                      and type Game.state = G.state =
struct
  structure Game = G
  structure EstOrd = OrderedExt(EstOrdered(Game))

  type edge = Game.move * Game.est
  datatype value = BestEdge of edge
		 | Pruned

  type alphabeta = value * value (* invariant: alpha < beta *)

  datatype result = Value of value
		  | ParentPrune   (* an evaluation result *)


  val search_depth = search_depth

  (* printing functions *)
  fun edge_toString (m,e) = "(" ^ G.move_toString m ^ ", " ^ G.est_toString e ^ ")"

  fun value_toString (Pruned: value): string = "Pruned"
    | value_toString (BestEdge (_,e)) = "Value(" ^ G.est_toString e ^ ")"

  fun alphabeta_toString (a,b) =
      "(" ^ value_toString a ^ "," ^ value_toString b ^ ")"

  fun result_toString (Value v) = value_toString v
    | result_toString ParentPrune = "ParentPrune"

  (* equality functions *)
  fun edge_eq ((m1,est1), (m2,est2)) =
    Game.move_eq (m1,m2) andalso Game.est_eq(est1,est2)

  fun value_eq (BestEdge e1, BestEdge e2): bool = edge_eq (e1,e2)
    | value_eq (Pruned, Pruned) = true
    | value_eq _ = false

  fun alphabeta_eq ((a1,b1): alphabeta, (a2,b2): alphabeta): bool =
      value_eq(a1,a2) andalso value_eq(b1,b2)

  fun result_eq (Value v1: result, Value v2: result): bool = value_eq (v1,v2)
    | result_eq (ParentPrune, ParentPrune) = true
    | result_eq _ = false


  (* for alpha, we want max(alpha,Pruned) to be alpha, i.e.
     Pruned <= alpha for any alpha;
     otherwise order by the estimates on the edges
     *)
  fun alpha_is_less_than (Pruned: value, v: Game.est): bool = true
    | alpha_is_less_than (BestEdge(_,alphav), v) = EstOrd.lt(alphav,v)

  fun maxalpha (Pruned, v2): value = v2
    | maxalpha (v1, Pruned) = v1
    | maxalpha (v1 as BestEdge(_,e1), v2 as BestEdge(_,e2)) =
       if EstOrd.lt (e1,e2) then v2 else v1

  (* for beta, we want min(beta,Pruned) to be beta, i.e.
     beta <= Pruned for any beta;
     otherwise order by the estimates on the edges
     *)
  fun beta_is_greater_than (v: Game.est, Pruned: value): bool = true
    | beta_is_greater_than (v, BestEdge(_,betav)) = EstOrd.lt(v,betav)

  fun minbeta (Pruned, v2): value = v2
    | minbeta (v1, Pruned) = v1
    | minbeta (v1 as BestEdge(_,e1), v2 as BestEdge(_,e2)) =
       if EstOrd.lt (e1,e2) then v1 else v2

(*-----------------------
  fun updateAB: state -> alplhabeta -> value -> alphabeta
  REQUIRES: true
  ENSURES: value of alphabeta is updated based on the new value passed in.
-------------------------*)

  fun updateAB (s: Game.state) ((a, b): alphabeta) (v1: value): alphabeta
    = case (Game.player (s)) of
         Game.Maxie => (maxalpha (a, v1), b)
      | Game.Minnie => (a, minbeta (b, v1))

(*! 
  REQUIRES: true
  ENSURES: case (Game.player (s)) of Game.Maxie  => if result = a then true else false
                                   | Game.Minnie => if result = b then true else false
!*)

  fun value_for (s: Game.state) ((a, b): alphabeta): value = 
    case (Game.player (s)) of
         Game.Maxie => a
      | Game.Minnie => b

(*!
  REQUIRES: true
  ENSURES: true
!*)

  fun check_bounds ((a, b): alphabeta) (s: Game.state) (x: Game.move)
      (y: Game.est): result = (let
         val MaxA = alpha_is_less_than (a, y)
         val MinB = beta_is_greater_than (y, b)
       in
         (case MaxA of
                    false => (case Game.player (s) of
                      Game.Maxie => ParentPrune
                      | Game.Minnie => Value (Pruned))
                | true => (case MinB of
                  false => (case Game.player (s) of
                      Game.Maxie => Value (Pruned)
                      | Game.Minnie => ParentPrune)
                  | true => Value (BestEdge (x, y))))
       end)

fun final_state (S: Game.State) = case (Game.status (S)) of
    Game.In_play => true
  | _ => false 

(*!
  REQUIRES: final_state (S) 
  ENSURES: true
!*)

  fun evaluate (0: int) ((a, b): alphabeta) (S: Game.state) 
      (Move: Game.move): result = (let
        val est = Game.estimate (S)
      in
        check_bounds ((a, b)) (S) (Move) (est)
      end) 

    | evaluate (n) ((a, b)) (S) (Move) = 
      case (Game.status (S)) of
        Game.In_play => (let
          val Val = search (n) ((a, b)) (S) (Game.moves (S))
        in
          case Val of
              Pruned => Value (Pruned)
            | BestEdge(m, e) => check_bounds ((a, b)) (S) (Move) (e)
        end)   
      | _ => check_bounds ((a, b)) (S) (Move) (Game.estimate (S))

  and

(*!
  REQUIRES: final_state (S) 
  ENSURES: true
!*)

  search (0: int) ((a, b): alphabeta) (S: Game.state) 
      (Moves: Game.move Seq.seq): value = raise Fail "Write-up lied to me!"

    | search (n) ((a,b)) (S) (Moves) = 
      if Seq.length (Moves) = 0 then value_for (S) ((a, b))
      else (let
      val Seq.Cons (First_Move, Rem_Moves) = Seq.showl (Moves)
    in
      case (evaluate (n - 1) ((a, b)) (Game.make_move (S, First_Move)) (First_Move)) of
          ParentPrune => Pruned 
        | Value (x) => 
          (let
            val (newa, newb) = updateAB (S) ((a, b)) (x) 
          in
            search (n) ((newa, newb)) (S) (Rem_Moves)
          end)
    end)

(*!
  REQUIRES: final_state (S)
  ENSURES: true
!*)

  fun next_move (S: Game.state): Game.move = let
    val v = search (search_depth) (Pruned, Pruned) 
                          (S) (Game.moves (S))
    in
      case v of
        BestEdge (m, e) => m
               | Pruned => raise Fail "This algorithm is bullshit"
    end

end (* functor MakeAlphaBeta *)
