(* andrew Id: _________________________________ *)

structure T3 = TicTacToe(val size = 4)

(* Players *)
structure HumanT3 = HumanPlayer(T3)
structure MM5T3   = MiniMax(structure G = T3
                            (* search depth 4 is relatively instantaneous;
                               5 takes about 5 seconds per move *)
                            val search_depth = 5)

structure AB5T3 = struct end (* AlphaBeta *)
structure J5T3  = struct end (* Jamboree  *)

(* Plays *)
structure T3_HvMM = Referee(structure Maxie = HumanT3
                            structure Minnie = MM5T3)

structure T3_HvAB = struct end (* AlphaBeta vs. human *)
structure T3_HvJ  = struct end (* Jamboree  vs. human *)
