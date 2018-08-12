signature FAIL = sig val assert : string * string * string * ( unit -> bool ) -> unit val fail : string * string * string -> 'a val unimplemented : string * string * string -> 'a  end ; structure Fail :> FAIL = struct 

 fun fail ( s , r , m ) = Assert.fail ( s ^ "." ^ r ^ ": " ^ m )  

 fun assert ( s , r , m , assert ) = if assert ( ) then ( ) else fail ( s , r , m )  

 fun unimplemented ( s , r , w ) = fail ( s , r , w ^ " unimplemented" )   end ; 

