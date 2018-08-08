structure Util :> UTIL = struct 

 fun last_elem ( m : int , [ ] : int list ) : bool = let val result = true in if ( true ) = false then raise Fail " Function last_elem Error: Requires Failure in line 7 " else if ( true ) = false then raise Fail " Function last_elem Error: Ensures Failure in line 8 " else true end  
 | last_elem ( m , x :: l ) = let val result = if m = x then last_elem ( m , l ) else false in if ( true ) = false then raise Fail " Function last_elem Error: Requires Failure in line 7 " else if ( true ) = false then raise Fail " Function last_elem Error: Ensures Failure in line 8 " else if m = x then last_elem ( m , l ) else false end  

 fun removeTrailing ( m : int , lis as [ ] : int list ) : int list = let val result = [ ] in if ( true ) = false then raise Fail " Function removeTrailing Error: Requires Failure in line 16 " else if ( case ( last_elem ( m , lis ) ) of true => ( List.length ( result ) <> List.length ( lis ) )| false => ( List.length ( result ) = List.length ( lis ) ) ) = false then raise Fail " Function removeTrailing Error: Ensures Failure in line 17 " else [ ] end  
 | removeTrailing ( m , lis as ( x :: l ) ) = let val result = if m = x andalso last_elem ( m , l ) then [ ] else [ x ] @ removeTrailing ( m , l ) in if ( true ) = false then raise Fail " Function removeTrailing Error: Requires Failure in line 16 " else if ( case ( last_elem ( m , lis ) ) of true => ( List.length ( result ) <> List.length ( lis ) )| false => ( List.length ( result ) = List.length ( lis ) ) ) = false then raise Fail " Function removeTrailing Error: Ensures Failure in line 17 " else if m = x andalso last_elem ( m , l ) then [ ] else [ x ] @ removeTrailing ( m , l ) end  

 fun ixmap_h ( f : int * 'a -> 'b ) ( n : int ) ( [ ] : 'a list ) : 'b list = let val result = [ ] in if ( true ) = false then raise Fail " Function ixmap_h Error: Requires Failure in line 26 " else if ( true ) = false then raise Fail " Function ixmap_h Error: Ensures Failure in line 27 " else [ ] end  
 | ixmap_h f n ( x :: l ) = let val result = f ( n , x ) :: ixmap_h f ( n + 1 ) l in if ( true ) = false then raise Fail " Function ixmap_h Error: Requires Failure in line 26 " else if ( true ) = false then raise Fail " Function ixmap_h Error: Ensures Failure in line 27 " else f ( n , x ) :: ixmap_h f ( n + 1 ) l end  

 fun ixmap ( f : int * 'a -> 'b ) ( [ ] : 'a list ) : 'b list = let val result = [ ] in if ( true ) = false then raise Fail " Function ixmap Error: Requires Failure in line 34 " else if ( true ) = false then raise Fail " Function ixmap Error: Ensures Failure in line 35 " else [ ] end  
 | ixmap f l = let val result = ixmap_h f 0 l in if ( true ) = false then raise Fail " Function ixmap Error: Requires Failure in line 34 " else if ( true ) = false then raise Fail " Function ixmap Error: Ensures Failure in line 35 " else ixmap_h f 0 l end  

 fun convolve_h ( n : int , 0 : int , acc : int , f : ( int -> int ) , g : ( int -> int ) ) : int = let val result = acc + ( f ( n ) * g ( 0 ) ) in if ( true ) = false then raise Fail " Function convolve_h Error: Requires Failure in line 42 " else if ( true ) = false then raise Fail " Function convolve_h Error: Ensures Failure in line 43 " else acc + ( f ( n ) * g ( 0 ) ) end  
 | convolve_h ( n , x , acc , f , g ) = let val result = convolve_h ( n + 1 , x - 1 , acc + ( f ( n ) * g ( x ) ) , f , g ) in if ( true ) = false then raise Fail " Function convolve_h Error: Requires Failure in line 42 " else if ( true ) = false then raise Fail " Function convolve_h Error: Ensures Failure in line 43 " else convolve_h ( n + 1 , x - 1 , acc + ( f ( n ) * g ( x ) ) , f , g ) end  

 fun convolve ( n : int , f : ( int -> int ) , g : ( int -> int ) ) : int = let val result = convolve_h ( 0 , n , 0 , f , g ) in if ( true ) = false then raise Fail " Function convolve Error: Requires Failure in line 52 " else if ( true ) = false then raise Fail " Function convolve Error: Ensures Failure in line 53 " else convolve_h ( 0 , n , 0 , f , g ) end  

 val [ ] = removeTrailing ( 1 , [ 1 , 1 , 1 , 1 , 1 ] ) 

 val [ 2 , 2 , 6 , 12 ] = ixmap ( fn ( i , x ) => 2 * i + ( round x ) ) [ 1.9 , 0.2 , 2.1 , 5.6 ] 

 val 80 = convolve ( 4 , fn n : int => n + 2 , fn n : int => n * n )  end 

