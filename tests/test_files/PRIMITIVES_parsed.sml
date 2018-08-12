structure Primitives : PRIMITIVES = struct 

 fun par ( f , g ) = ( f ( ) , g ( ) )  

 fun par3 ( f , g , h ) = ( f ( ) , g ( ) , h ( ) )  

 fun parTab ( n , f ) = let 

 val v = Vector.tabulate ( n , f )  in fn i => Vector.sub ( v , i ) end   end 

