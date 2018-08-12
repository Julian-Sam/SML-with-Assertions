signature DATAFLOW = sig type analysis type node type info type result = node -> { iInfo : info , oInfo : info } val analyze : analysis -> result  end functor DataFlowF ( type analysis type node structure NodeDict : DICT where type key = node structure Info : sig type info type infoRef val refMk : info -> infoRef val refGet : infoRef -> info  end val initial : analysis * node -> bool val initialVal : analysis -> Info.info val bottom : analysis -> Info.info val components : analysis -> node List.t List.t val successors : analysis * node -> node List.t val transfer : analysis * node -> ( Info.info * Info.infoRef -> bool )  ) : DATAFLOW where type node = node and type info = Info.info and type analysis = analysis = struct 

 type node = node 

 type info = Info.info 

 type analysis = analysis 

 type result = node -> { iInfo : info , oInfo : info } 

 datatype item = I of { node : node , input : Info.infoRef , transfer : Info.info * Info.infoRef -> bool , active : bool ref , successors : item List.t ref } structure ND = NodeDict 

 val fail = fn ( f , msg ) => Fail.fail ( "DataFlowF" , f , msg ) 

 val orderComponents : node List.t List.t -> node List.t List.t = fn ccs => ccs 

 val foldCCs = fn ( ccs , init , doIt ) => List.fold ( ccs , init , fn ( cc , d ) => List.fold ( cc , d , doIt ) ) 

 val mapCCs = fn ( ccs , doIt ) => List.map ( ccs , fn cc => List.map ( cc , doIt ) ) 

 val foreachCCs = fn ( ccs , doIt ) => List.foreach ( ccs , fn cc => List.foreach ( cc , doIt ) ) 

 val initialize : analysis -> item List.t List.t = fn analysis => let 

 val initialVal = initialVal analysis 

 val bottom = bottom analysis 

 val ccs = components analysis 

 val ccs = orderComponents ccs 

 val ccs = let 

 val doIt = fn node => I { node=node , input=Info.refMk ( if initial ( analysis , node ) then initialVal else bottom ) , transfer=transfer ( analysis , node ) , active=ref true , successors=ref [ ] }  in mapCCs ( ccs , doIt ) end 

 val getItem = let 

 val doIt = fn ( item as I { node , ... } , d ) => ND.insert ( d , node , item ) 

 val d = foldCCs ( ccs , ND.empty , doIt )  in fn n => case ND.lookup ( d , n ) of SOME i => i| NONE => fail ( "initialize" , "Graph is not closed" ) end 

 val () = let 

 val doIt = fn ( I { node , successors = ssR , ... } ) => let 

 val ss = successors ( analysis , node ) 

 val ssItems = List.map ( ss , getItem ) 

 val () = ssR := ssItems  in ( ) end  in foreachCCs ( ccs , doIt ) end  in ccs end 

 val propagateEdge : item * item -> unit = fn ( I { input = i1 , transfer , ... } , I { active , input = i2 , ... } ) => if transfer ( Info.refGet i1 , i2 ) then active := true else ( ) 

 val propagate : item -> unit = fn ( i1 as I { successors , ... } ) => List.foreach ( ! successors , fn i2 => propagateEdge ( i1 , i2 ) ) 

 val step1 : item -> bool = fn ( item as I { active , ... } ) => let 

 val b = ! active 

 val () = active := false 

 val () = if b then propagate item else ( )  in b end 

 val step : item List.t -> bool = fn cc => let 

 val doOne = fn ( item , b ) => let 

 val active = step1 item  in b orelse active end  in List.fold ( cc , false , doOne ) end 

 val iterate1 : item List.t -> unit = fn cc => let 

 val rec loop = fn () => if step cc then loop ( ) else ( )  in loop ( ) end 

 val iterate : item List.t List.t -> unit = fn ccs => List.foreach ( ccs , iterate1 ) 

 val extract : item List.t List.t * info -> result = fn ( ccs , bottom ) => let 

 val doOne = fn ( I { node , input , transfer , active , ... } , d ) => let 

 val () = if ! active then fail ( "extract" , "Didn't properly reach a fixed point" ) else ( ) 

 val iInfo = Info.refGet input 

 val output = Info.refMk bottom 

 val _ = transfer ( iInfo , output ) 

 val oInfo = Info.refGet output  in ND.insert ( d , node , { iInfo=iInfo , oInfo=oInfo } ) end 

 val d = foldCCs ( ccs , ND.empty , doOne ) 

 val result = fn n => ( case ND.lookup ( d , n ) of SOME r => r| NONE => fail ( "extract" , "Not a node" ) )  in result end 

 val analyze : analysis -> result = fn analysis => let 

 val ccs = initialize analysis 

 val () = iterate ccs 

 val result = extract ( ccs , bottom analysis )  in result end  end 

