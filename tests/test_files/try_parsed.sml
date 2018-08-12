signature TRY = sig type 'a t = 'a val lift : ( 'a -> 'b t ) -> ( 'a -> 'b option ) val try : ( unit -> 'a t ) -> 'a option val exec : ( unit -> unit t ) -> unit val || : ( 'a -> 'b t ) * ( 'a -> 'b t ) -> ( 'a -> 'b option ) val or : ( 'a -> 'b option ) * ( 'a -> 'b option ) -> ( 'a -> 'b option ) val orL : ( 'a -> 'b option ) list -> ( 'a -> 'b option ) val oo : ( 'b -> 'c option ) * ( 'a -> 'b option ) -> ( 'a -> 'c option ) val om : ( 'b -> 'c t ) * ( 'a -> 'b option ) -> ( 'a -> 'c option ) val combine : ( 'a -> 'b t ) list * ( 'a -> 'b ) -> ( 'a -> 'b ) val combineDo : ( unit -> 'a t ) list * ( unit -> 'a ) -> 'a val fail : unit -> 'a t val <- : 'a option -> 'a t val <@ : ( 'a -> 'b option ) -> ( 'a -> 'b t ) val << : ( 'b -> 'c option ) * ( 'a -> 'b option ) -> ( 'a -> 'c t ) val <! : ( 'b -> 'c t ) * ( 'a -> 'b option ) -> ( 'a -> 'c t ) val require : bool -> unit t structure V : sig val sub : 'a Vector.t * int -> 'a t val singleton : 'a Vector.t -> 'a t val doubleton : 'a Vector.t -> ( 'a * 'a ) t val tripleton : 'a Vector.t -> ( 'a * 'a * 'a ) t val lenEq : 'a Vector.t * int -> unit t val isEmpty : 'a Vector.t -> unit t  end  end structure Try :> TRY = struct 

 type 'a t = 'a  exception Fail 

 val lift = fn f => fn a => ( SOME ( f a ) ) handle Fail => NONE 

 val try = fn f => lift f ( ) 

 val exec = fn f => ignore ( try f ) 

 fun || ( f , g ) = fn arg => ( case try ( fn () => f arg ) of NONE => try ( fn () => g arg )| ans => ans )  

 val or = fn ( f1 , f2 ) => fn args => ( case f1 args of NONE => f2 args| r => r ) 

 fun orL fs = List.fold ( fs , fn _ => NONE , or )  

 val oo = fn ( f , g ) => fn x => ( case g x of SOME y => f y| NONE => NONE ) 

 val om = fn ( f , g ) => fn x => ( case g x of SOME y => SOME ( f y )| NONE => NONE ) 

 fun combine ( fs , g ) = List.foldr ( fs , g , fn ( f , g ) => fn arg => case try ( fn () => f arg ) of NONE => g arg| SOME res => res )  

 fun combineDo ( fs , g ) = List.foldr ( fs , g , fn ( f , g ) => fn () => case try f of NONE => g ( )| SOME res => res ) ( )  

 fun fail () = raise Fail  

 fun <- t = ( case t of SOME a => a| NONE => raise Fail )  

 val <@ = fn f => fn x => <- ( f x ) 

 val << = fn ( f , g ) => ( <@ f ) o ( <@ g ) 

 val <! = fn ( f , g ) => f o <@ g 

 val require = fn b => if b then ( ) else fail ( ) structure V = struct 

 fun sub ( v , i ) = let 

 val () = require ( ( i >= 0 ) andalso ( i < Vector.length v ) ) 

 val r = Vector.sub ( v , i )  in r end  

 fun singleton v = let 

 val () = require ( Vector.length v = 1 ) 

 val r = Vector.sub ( v , 0 )  in r end  

 fun doubleton v = let 

 val () = require ( Vector.length v = 2 ) 

 val r1 = Vector.sub ( v , 0 ) 

 val r2 = Vector.sub ( v , 1 )  in ( r1 , r2 ) end  

 fun tripleton v = let 

 val () = require ( Vector.length v = 3 ) 

 val r1 = Vector.sub ( v , 0 ) 

 val r2 = Vector.sub ( v , 1 ) 

 val r3 = Vector.sub ( v , 2 )  in ( r1 , r2 , r3 ) end  

 fun lenEq ( v , i ) = require ( Vector.length v = i )  

 val isEmpty = fn v => lenEq ( v , 0 )  end  end 

