(* andrew Id: ___sjahmad____ *)

functor MakeJamboree (structure G : GAME
                      val search_depth : int
                      val prune_percentage : real)
        : JAMBOREE where type Game.move = G.move
                   where type Game.state = G.state =
struct
  structure Game = G
  structure EstOrd = OrderedExt(EstOrdered(Game))

  type edge = Game.move * Game.est
  datatype value = BestEdge of edge
		 | Pruned

  type alphabeta = value * value (* invariant: alpha < beta *)

  datatype result = Value of value
		  | ParentPrune   (* an evaluation result *)


  val search_depth     = search_depth
  val prune_percentage = prune_percentage

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

(*-----------------------
  fun value_for: state -> alplhabeta -> value 
  REQUIRES: true
  ENSURES: if Maxie State then alpha is returned else beta.
-------------------------*)

  fun value_for (s: Game.state) ((a, b): alphabeta): value = 
    case (Game.player (s)) of
         Game.Maxie => a
      | Game.Minnie => b

(*-----------------------
  fun check_bounds: alplhabeta -> state -> move -> est -> result 
  REQUIRES: true
  ENSURES: if est is in bounds then returns it as BestEdge (m , e)
          else prunes or parent prunes based on the player.
-------------------------*)

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

(*-----------------------
  fun evaluate: int -> alphabeta -> state -> move -> result
  and search:   int -> alphabeta -> state -> move Seq.seq -> move Seq.seq -> value
  REQUIRES: state not final state
  ENSURES: value returned is optimal for player at called state.
-------------------------*)

  fun evaluate (0: int) ((a, b): alphabeta) (S: Game.state) 
      (Move: Game.move): result = (let
        val Est = Game.estimate (S)
      in
        check_bounds ((a, b)) (S) (Move) (Est)
      end) 

    | evaluate (n) ((a, b)) (S) (Move) = 
      case (Game.status (S)) of
        Game.In_play => (let
          val Moves = Game.moves (S)
          val Length = Seq.length (Moves)
          val (abmoves, mmmoves) = Seq.split 
              (Real.floor (prune_percentage * Real.fromInt(Length))) (Moves)

          val Val = search (n) ((a, b)) (S) (abmoves) (mmmoves)
        in
          case Val of
              Pruned => Value (Pruned)
            | BestEdge(m, e) => check_bounds ((a, b)) (S) (Move) (e)
        end)   
      | _ => check_bounds ((a, b)) (S) (Move) (Game.estimate (S))


  and search (0: int) ((a, b): alphabeta) (S: Game.state) 
      (abmoves: Game.move Seq.seq) (mmmoves: Game.move Seq.seq): value = 
      raise Fail "Write-up lied to me!"

    | search (n) ((a,b)) (S) (abmoves) (mmmoves) = 
      if Seq.length (abmoves) = 0 then let
        val result = Seq.mapreduce (fn m => evaluate (n - 1) ((a, b)) 
                                            (Game.make_move(S, m)) (m)) 
               (Value (value_for (S) ((a, b)))) (fn (x, y) => case (x, y) of
                 (ParentPrune, _) => ParentPrune
                 | (_, ParentPrune) => ParentPrune
                 | (Value (a), Value (b)) => case (Game.player (S)) of
                    Game.Maxie => Value (maxalpha (a, b))
                   | Game.Minnie => Value (minbeta (a, b))) (mmmoves)
      in
        case result of
          ParentPrune => Pruned
          | Value (x) => x
      end
      
      else (let
      val Seq.Cons (First_Move, Rem_Moves) = Seq.showl (abmoves)
    in
      case (evaluate (n - 1) ((a, b)) (Game.make_move (S, First_Move)) (First_Move)) of
          ParentPrune => Pruned 
        | Value (x) => 
          (let
            val (newa, newb) = updateAB (S) ((a, b)) (x) 
          in
            search (n) ((newa, newb)) (S) (Rem_Moves) (mmmoves)
          end)
    end)

(*-----------------------
  fun next_move: state -> move
  REQUIRES: state not final state.
  ENSURES: Next move most optimal for player in state according to alphabeta pruning
-------------------------*)

  fun next_move (S: Game.state): Game.move = let
    val Moves = Game.moves (S)
    val Length = Seq.length (Moves)
    val (abmoves, mmmoves) = Seq.split 
        (Real.floor (prune_percentage * Real.fromInt(Length))) (Moves)
    val v = search (search_depth) (Pruned, Pruned) 
                          (S) (abmoves) (mmmoves)
    in
      case v of
        BestEdge (m, e) => m
               | Pruned => raise Fail "This algorithm is bullshit"
    end

end (* functor MakeJamboree *)
