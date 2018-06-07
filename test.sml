fun NICE (_, 0) = 0
  | NICE (0, _) = 0
  | NICE (x, y) = case (x*y) of 1 => "true"
   	 		         | 0 => "false"; 