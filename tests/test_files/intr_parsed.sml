signature INTR = sig type t val empty : t val all : t val nat : t val range : { lower : IntInf.t option , upper : IntInf.t option } -> t val interval : { lower : IntInf.t , upper : IntInf.t } -> t val single : IntInf.t -> t val fromInt : int -> t val toIntInf : t -> IntInf.t option val toInt : t -> int option val check : t -> bool val isEmpty : t -> bool val intersection : t * t -> t val out : t -> { lower : IntInf.t option , upper : IntInf.t option } val layout : t -> Layout.t  end ; structure Intr :> INTR = struct 

 datatype t = IR of { lower : IntInf.t option , upper : IntInf.t option } 

 val empty = IR { lower=SOME IntInf.one , upper=SOME IntInf.zero } 

 val all = IR { lower=NONE , upper=NONE } 

 val nat = IR { lower=SOME IntInf.zero , upper=NONE } 

 fun range { lower , upper } = IR { lower=lower , upper=upper }  

 fun interval { lower , upper } = IR { lower=SOME lower , upper=SOME upper }  

 fun single i = IR { lower=SOME i , upper=SOME i }  

 fun fromInt i = single ( IntInf.fromInt i )  

 fun toIntInf ( IR { upper , lower } ) = case ( lower , upper ) of ( SOME l , SOME u ) => if IntInf.equals ( l , u ) then SOME l else NONE| _ => NONE  

 fun toInt ir = Option.map ( toIntInf ir , IntInf.toInt )  

 fun check _ = true  

 fun isEmpty ( IR { upper , lower } ) = case ( upper , lower ) of ( SOME l , SOME u ) => IntInf.> ( l , u )| _ => false  

 fun isSingle ( IR { upper , lower } ) = case ( upper , lower ) of ( SOME l , SOME u ) => IntInf.equals ( l , u )| _ => false  

 fun intersection ( ir1 , ir2 ) = let 

 val IR { lower = l1 , upper = u1 } = ir1 

 val IR { lower = l2 , upper = u2 } = ir2 

 val l = case ( l1 , l2 ) of ( NONE , x ) => x| ( x , NONE ) => x| ( SOME l1 , SOME l2 ) => SOME ( IntInf.max ( l1 , l2 ) ) 

 val u = case ( u1 , u2 ) of ( NONE , x ) => x| ( x , NONE ) => x| ( SOME u1 , SOME u2 ) => SOME ( IntInf.min ( u1 , u2 ) ) 

 val ir = IR { lower=l , upper=u }  in if isEmpty ir then empty else ir end  

 fun out ( IR { lower , upper } ) = { lower=lower , upper=upper }  local structure L = Layout structure LU = LayoutUtils  in 

 fun layout ( ir as IR { lower , upper } ) = let 

 val ll = case lower of NONE => L.str "-w"| SOME i => IntInf.layout i 

 val lu = case upper of NONE => L.str "w"| SOME i => IntInf.layout i 

 val l = if isSingle ir then ll else L.seq [ ll , L.str ".." , lu ] 

 val l = if isEmpty ir then L.empty else l 

 val l = LU.bracket l  in l end   end  end ; 

