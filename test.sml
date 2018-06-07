fun solid (_, 0) = 0
  | solid (0, _) = 0
  | solid (x, y) = x + y;