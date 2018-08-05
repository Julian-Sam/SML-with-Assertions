(* andrew Id: ___sjahmad___ *)

functor MakeTicTacToe (val size: int): TICTACTOE =
struct
  datatype player  = Minnie
		   | Maxie

  datatype outcome = Winner of player
		   | Draw

  datatype status  = Over of outcome
		   | In_play

  datatype est     = Definitely of outcome
	           | Guess of int

  datatype position = Filled of player
		    | Empty

  (* (0,0) is bottom-left corner *)
  datatype c4state = Unimplemented (* use in starter code only *)
        |  S of (position Matrix.matrix) * player
  type state = c4state

  type move = int * int (* cols and rows are numbered 0 ... num_cols-1 *)

  val num_cols = size
  val num_rows = size

  (* printing functions *)
  fun player_toString Maxie  = "Maxie"
    | player_toString Minnie = "Minnie"

  fun outcome_toString (Winner P) = player_toString P ^ " wins!"
    | outcome_toString Draw       = "It's a draw!"

  fun status_toString (Over O) = "Over ("^ outcome_toString O ^")"
    | status_toString In_play  = "Still playing"

  fun est_toString (Definitely O) = outcome_toString O
    | est_toString (Guess i)      = "Guess: " ^ Int.toString i

  fun move_toString (x,y) = "("^(Int.toString x)^","^(Int.toString y)^")"

  fun pos_toString (Filled Minnie) = "O"
    | pos_toString (Filled Maxie)  = "X"
    | pos_toString Empty           = " "

  fun state_toString (board as (S (m, _))) =
      let
        val rows = Matrix.rows m
        val ts : string Seq.seq -> string = Seq.reduce op^ ""
        fun print_row s =
            "|" ^ ts (Seq.map (fn x => pos_toString x ^ "|") s) ^ "\n"
      in
          "-" ^ ts (Seq.tabulate (fn _ => "--") num_cols) ^ "\n" ^
          Seq.mapreduce print_row "" (fn (x,y) => y^x) rows ^ "\n"
      end
    | state_toString Unimplemented = raise Fail "Incomplete implementation"


  (* equality functions *)
  val player_eq  = (op=)
  val outcome_eq = (op=)
  val status_eq  = (op=)
  val est_eq     = (op=)
  val move_eq    = (op=)

  fun pos_eq (Filled p1, Filled p2) = p1=p2
    | pos_eq (Empty, Empty) = true
    | pos_eq _ = false

  fun state_eq (S (m1,p1), S (m2,p2)) =
    Matrix.eq pos_eq (m1,m2) andalso p1=p2


  (* parsing functions *)
  fun parse_move (S(st,p)) input =
    let
        val [x,y] = String.tokens (fn #"," => true | _ => false) (input)
        val SOME(x) = Int.fromString(String.extract (x,1,NONE))
        val SOME(y) = Int.fromString(y)
    in
      if x >= 0 andalso x < num_cols andalso y >= 0 andalso y < num_rows
      then (case (Matrix.sub (st) (x,y))
              of Filled(_) => NONE
               | _ => SOME(x,y))
      else NONE
    end


  val start = S (Matrix.repeat (Empty) (num_rows, num_cols), Maxie)

(*-----------------------
  fun make_move: state * move -> state
  REQUIRES: move valid
  ENSURES: state updated with the move inputted
-------------------------*)

  fun make_move (S (Mat, P): state, (x, y): move): state = let
      val NewP = (case P of
                  Maxie => Minnie
                | _     => Maxie)
    in
      (case (Matrix.sub (Mat) (x,y)) of
      Filled _ => raise Fail "Position Invalid"
      | Empty => S (Matrix.update (Mat) ((x,y), Filled (P)), NewP))
    end
      
(*-----------------------
  fun moves: state -> move Seq.seq
  REQUIRES: true
  ENSURES: Gives sequence of all moves that can be done one step from this state.
-------------------------*)

  fun moves (S (Mat, P): state): move Seq.seq = 
    Matrix.matching_subs (fn x: position => case (x) of
      Empty => true
       |  _ => false) (Mat) 


(*----------   CODE FOR STATUS STARTS HERE   ------------*)

(*-----------------------
  fun Equal_Rec: int * position Seq.seq * (position Seq.seq) Seq.seq -> bool * player
  REQUIRES: int >= 0
  ENSURES: If a player won.
-------------------------*)

  fun Equal_Rec (0: int, Lone_Seq: position Seq.seq, 
      Rem_Seqs: (position Seq.seq) Seq.seq): bool * player = (false, Maxie)
    
    | Equal_Rec (count, Seq1, Seqs) = let
      val Check_Maxie  = Seq.all (fn x => case x of
                          Filled Maxie => true
                                   | _ => false) (Seq1) 
      val Check_Minnie = Seq.all (fn x => case x of
                         Filled Minnie => true
                                   | _ => false) (Seq1) 
      val (Seq2, NewSeqs) = Seq.split (1) (Seqs)
    in
      if Check_Maxie then (true, Maxie)
      else if Check_Minnie then (true, Minnie)
      else Equal_Rec (count - 1, Seq.flatten (Seq2), NewSeqs)
    end

(*-----------------------
  fun n_in_rows: position Seq.seq Seq.seq -> bool * player
  REQUIRES: true
  ENSURES: if player has a full row then return (true, player) else return false.
-------------------------*)

  fun n_in_rows (Seqs: (position Seq.seq) Seq.seq): bool * player = let
      val (Seq2, NewSeqs) = Seq.split (1) (Seqs)
    in
      Equal_Rec (size - 1, Seq.flatten (Seq2), NewSeqs)
    end

(*-----------------------
  fun n_in_cols: position Seq.seq Seq.seq -> bool * player
  REQUIRES: true
  ENSURES: if player has a full column then return (true, player) else return false.
-------------------------*)

  fun n_in_cols (Seqs: (position Seq.seq) Seq.seq): bool * player = let
      val (Seq2, NewSeqs) = Seq.split (1) (Seqs)
    in
      Equal_Rec (size - 1, Seq.flatten (Seq2), NewSeqs)
    end

(*-----------------------
  fun Get_Seq_Int: int (1) * position Seq.seq * position Seq.seq Seq.seq (2) -> position Seq.seq
  REQUIRES: int >= 0
  ENSURES: Get Sequence from (2) at index (1)
-------------------------*)

  fun Get_Seq (0: int, Lone_Seq: position Seq.seq, 
      Rem_Seqs: position Seq.seq Seq.seq): position Seq.seq = Lone_Seq
    | Get_Seq (count, Seq1, Seqs) = let
      val (Seq2, NewSeqs) = Seq.split (1) (Seqs) 
    in
      Get_Seq (count - 1, Seq.flatten(Seq2), NewSeqs)
    end

(*-----------------------
  fun n_in_diags: position Seq.seq Seq.seq * position Seq.seq Seq.seq (2) -> (bool * player)
  REQUIRES: int >= 0
  ENSURES: returns true if a player has Filled all diags and false otherwise
-------------------------*)

  fun n_in_diags (Seqs1: (position Seq.seq) Seq.seq, 
                  Seqs2: (position Seq.seq) Seq.seq): (bool * player) = let
      val Largest_Diag1 = Get_Seq (size - 1, Seq.empty(), Seqs1)
      val Largest_Diag2 = Get_Seq (size - 1, Seq.empty(), Seqs2)
    in
      (case (Seq.all (fn x => case x of
              Filled Maxie => true
                       | _ => false) (Largest_Diag1)) of
                  true => (true, Maxie)
                  | false => (case (Seq.all (fn x => case x of
                                    Filled Minnie => true
                                              | _ => false) (Largest_Diag1)) of
                    true => (true, Minnie)
                    | false => 
      (case (Seq.all (fn x => case x of
              Filled Maxie => true
                       | _ => false) (Largest_Diag2)) of
                  true => (true, Maxie)
                  | false => (case (Seq.all (fn x => case x of
                                    Filled Minnie => true
                                              | _ => false) (Largest_Diag2)) of
                    true => (true, Minnie)
                    | false => (false, Maxie)))))
    end

(*-----------------------
  fun status: state -> status
  REQUIRES: true
  ENSURES: returns Over(Winner x) if player x has won orelse returns Over (Draw) if
  no more moves can be made else returns In_play
-------------------------*)

  fun status (S(Mat, P): state): status = let
        val RowSeq = Matrix.rows (Mat)
        val ColSeq = Matrix.cols (Mat)
        val (BL2TR, BR2TL)  = (Matrix.diags1 (Mat), Matrix.diags2 (Mat))
        val BoolMat = Matrix.map (fn x => (case x of
                          Filled _ => false
                               | _ => true)) (Mat)
        val BoolSeq = Seq.flatten (Matrix.rows (BoolMat))
      in
        (case (n_in_rows RowSeq) of
          (true, P1) => Over (Winner P1)
        | (false, _) => (case (n_in_cols ColSeq) of
                          (true, P1) => Over (Winner P1)
                        | (false, _) => (case (n_in_diags (BL2TR, BR2TL)) of
                                            (true, P1) => Over (Winner P1)
                                          | (false, _) => 
                        (case (Seq.exists (fn x => x) (BoolSeq)) of
                          true => In_play
                          | false => Over (Draw)))))
      end

(*----------   CODE FOR STATUS ENDS HERE   ------------*)

(*-----------------------
  fun player: state -> player
  REQUIRES: true
  ENSURES: Player whose current turn it is returns
-------------------------*)

  fun player (S (Mat, P): state): player = P

(*-----------------------
  fun Get_Seq_Int: int (1) * int Seq.seq * int Seq.seq Seq.seq (2) -> int Seq.seq
  REQUIRES: int >= 0
  ENSURES: Get Sequence from (2) at index (1)
-------------------------*)

  fun Get_Seq_Int (0: int, Lone_Seq: int Seq.seq, 
      Rem_Seqs: int Seq.seq Seq.seq): int Seq.seq = Lone_Seq
    | Get_Seq_Int (count, Seq1, Seqs) = let
      val (Seq2, NewSeqs) = Seq.split (1) (Seqs) 
    in
      Get_Seq_Int (count - 1, Seq.flatten(Seq2), NewSeqs)
    end

(*-----------------------
  fun check_rows: int (1) * int Seq.seq Seq.seq (2) * int -> int
  REQUIRES: int (1) >= 0
  ENSURES: Maps each int row to their respective Guess values.
-------------------------*)

  fun check_rows (0: int, Seq: (int Seq.seq) Seq.seq, acc: int): int =
      acc
    | check_rows (n, Seqs, acc) = let
      val Seq.Cons (Row1, Rest_Rows) = Seq.showl (Seqs)
      val Eval = Seq.reduce (fn (x, y) => x + y) (0) (Row1)
    in
      case Eval of
        4 => check_rows (n - 1, Rest_Rows, acc + 1)
        |  3 => check_rows (n - 1, Rest_Rows, acc + 3)
        | ~3 => check_rows (n - 1, Rest_Rows, acc - 3)
        | ~4 => check_rows (n - 1, Rest_Rows, acc - 1)
        | x => check_rows (n - 1, Rest_Rows, (acc * x) div size)
    end

(*-----------------------
  fun check_cols: int (1) * int Seq.seq Seq.seq (2) * int -> int
  REQUIRES: int (1) >= 0
  ENSURES: Maps each int column to their respective Guess values.
-------------------------*)

  fun check_cols (0: int, Seq: (int Seq.seq) Seq.seq, acc: int): int =
      acc
    | check_cols (n, Seqs, acc) = let
      val Seq.Cons (Row1, Rest_Rows) = Seq.showl (Seqs)
      val Eval = Seq.reduce (fn (x, y) => x + y) (0) (Row1)
    in
      case Eval of
           4 => check_cols (n - 1, Rest_Rows, acc + 4)
        |  3 => check_cols (n - 1, Rest_Rows, acc + 3)
        | ~3 =>check_cols (n - 1, Rest_Rows, acc - 3)
        | ~4 => check_cols (n - 1, Rest_Rows, acc - 4)
        |  x => check_cols (n - 1, Rest_Rows, (acc * x) div size)
    end  

(*-----------------------
  fun check_diags: int (1) * int Seq.seq Seq.seq (2) * int Seq.seq Seq.seq (3) -> int
  REQUIRES: int (1) >= 0
  ENSURES: Maps each diagonal to a respective guess value
-------------------------*)

    fun check_diags (n: int, Seq1: (int Seq.seq) Seq.seq, 
                             Seq2: (int Seq.seq) Seq.seq, acc: int): int =
    let
      val Largest_Diag1 = Get_Seq_Int (size - 1, Seq.empty(), Seq1)
      val Largest_Diag2 = Get_Seq_Int (size - 1, Seq.empty(), Seq2)
      val Eval1 = Seq.reduce (fn (x, y) => x + y) (0) (Largest_Diag1)
      val Eval2 = Seq.reduce (fn (x, y) => x + y) (0) (Largest_Diag2)
    in
      case (Eval1, Eval2) of 
          (4, 4) => 8
      |   (3, 3) => (case (Seq.exists (fn 0 => true | _ => false) (Largest_Diag1)) of
              true => (case (Seq.exists (fn 0 => true | _ => false) (Largest_Diag2)) of
                        true => 6
                        | false => 3)
              | false => (case (Seq.exists (fn 0 => true | _ => false) (Largest_Diag2)) of
                        true => 3
                        | false => 0))
                    
      | (~3, ~3) => (case (Seq.exists (fn 0 => true | _ => false) (Largest_Diag1)) of
              true => (case (Seq.exists (fn 0 => true | _ => false) (Largest_Diag2)) of
                        true => ~6
                        | false => ~3)
              | false => (case (Seq.exists (fn 0 => true | _ => false) (Largest_Diag2)) of
                        true => ~3
                        | false => 0))
      | (~4, ~4) => ~8
      |   (x, y) =>  (acc * (x + y)) div (2 * size)
    end

(*-----------------------
  fun easy_win: state -> bool
  REQUIRES: true
  ENSURES: returns true if any next_move can give final move for player of state.
-------------------------*)

  fun easy_win (s: state): bool = let
    val Moves = moves (s)
    val States = Seq.map (fn x => make_move (s, x)) (Moves)
  in
    if Seq.exists (fn x => case (status (x)) of
      Over (x) => true
      | In_play => false) (States) then true else false
  end

(*-----------------------
  fun estimate: state -> est
  REQUIRES: true
  ENSURES: returns a value for the state that guesses its likeliness in terms of Maxie winning and Minnie winning.
-------------------------*)

  fun estimate (S (Mat, P): state): est = let
      val MappedMat = Matrix.map (fn x => case x of
          Filled (Maxie) => 1
        | Filled (Minnie) => ~1
        |           Empty => 0) (Mat)
    in
      case (status (S(Mat, P))) of
        Over (x) => Definitely (x)
        | In_play => if (easy_win (S (Mat, P))) then Definitely (Winner (P))
      else (let
        val Acc = check_rows (size, Matrix.rows (MappedMat), 0)
        val Acc2 = check_cols (size, Matrix.rows (MappedMat), Acc)
        val Acc3 = check_diags (size - 1, Matrix.diags1 (MappedMat), 
                                          Matrix.diags2 (MappedMat), Acc2)
      in
        Guess (Acc3)
      end)
    end
    

end (* functor TicTacToe *)
