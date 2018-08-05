(* andrew Id: ___sjahmad___ *)

datatype instruction = Right
                     | Step of int

type itinerary = instruction list

datatype orientation = North | East | South | West

type position = int * int * orientation

(*
  fun turnRight: position |----> position

  REQUIRES input to be of type position.
  ENSURES that the result is a position and that orientation is
  rotated by 90 degrees clockwise.
*)

fun turnRight ((x, y, North): position): position = (x, y, East)
  | turnRight ((x, y, East)) = (x, y, South)
  | turnRight ((x, y, South)) = (x, y, West)
  | turnRight ((x, y, West)) = (x, y, North)

val (0, 0, East) = turnRight((0, 0, North))
(*
  fun move: int * position |----> position

  REQUIRES input to be of type int and position respectively.
  ENSURES that the result is still a position which has shifted 
  n units to the oriented direction
*)

fun move (0: int, (x, y, Ott): position): position = (x, y, Ott)
  | move (a, (x, y, North)) = (x, y + a, North)
  | move (a, (x, y, East))  = (x + a, y, East)
  | move (a, (x, y, South)) = (x, y - a, South)
  | move (a, (x, y, West))  = (x - a, y, West)

val (0, 2, North): position = move(2, (0, 0, North))
val (4, 3, South): position = move(~2, (4, 1, South))


(*
  fun getPosition: itinerary * position |----> position

  REQUIRES input to be of type itinerary and position respectively.
  ENSURES that the result is a position that is obtained by moving current
  position by reading the instructions from the itinerary.
*)

fun getPosition ([]: itinerary, (x, y, Otto): position): position = 
	(x, y, Otto)
  | getPosition (Step a :: il, (x, y, Otto)) = getPosition (il, move (a, (x, y, Otto))) 
  | getPosition (Right :: il, (x, y, Otto)) = getPosition (il, turnRight (x, y, Otto))

val (0, 0, North) = getPosition([], (0, 0, North))
val (2, 4, East) = getPosition([Step 2, Right, Step 2, Right, Right, Right, Step 2, Right], (0, 0, North))
(*
  fun tgoBackh: itinerary * itinerary |----> itinerary

  REQUIRES inputs to be of type itinerary.
  ENSURES that the result is an itinerary that includes steps to reverse
  the pathway projected by the input itinerary.
*)

fun tgoBackh ([]: itinerary, y: itinerary): itinerary = y
  | tgoBackh (Step a :: l, y) = tgoBackh(l, [Step (~a)] @ y )
  | tgoBackh (Right :: l, y) = tgoBackh(l, [Right, Right, Right] @ y)

val [] = tgoBackh ([], [])
val [Right, Right, Right,
     Step ~1, Right, Right, Right] = tgoBackh([Right, Step 1, Right], [])

(*
  fun goBack: itinerary |----> itinerary

  REQUIRES input to be of type itinerary.
  ENSURES that the result is an itinerary that includes steps to reverse
  the pathway projected by the input itinerary
*)

fun goBack (x: itinerary): itinerary = tgoBackh (x, [])

val [] = goBack([])
val [Right, Right, Right, Right,
     Right, Right, Step ~6, Right,
     Right, Right, Step ~3] = goBack([Step 3, Right, Step 6, Right, Right])

