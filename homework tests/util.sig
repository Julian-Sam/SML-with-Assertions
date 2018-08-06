signature UTIL =
sig

    val removeTrailing : int * int list -> int list
    val ixmap : (int * 'a -> 'b) -> 'a list -> 'b list
    val convolve : int * (int -> int) * (int -> int) -> int

end














