signature MIL_SHAPE_ANALYSIS = sig type t type env datatype value = VSymb of Identifier.variable | VInt of Int32.t val debugs : Config.Debug.debug list val length : t * Identifier.variable -> value option val isScalar : t * Identifier.variable -> bool val isArray : t * Identifier.variable -> bool val equalLength : t * Identifier.variable * Identifier.variable -> bool val layout : t -> Layout.t val program : env * Mil.t -> t  end functor MilShapeAnalysisF ( type env val getConfig : env -> Config.t val passname : string val indent : int  ) :> MIL_SHAPE_ANALYSIS where type env = env = struct 

 val myPassname = passname ^ ":SAnlz" 

 val ( debugPassD , debugPass ) = Config.Debug.mk ( myPassname , "debug the shape analysis module" ) 

 type env = env structure L = Layout structure LU = LayoutUtils structure M = Mil structure I = Identifier structure LS = Identifier.LabelSet structure LD = Identifier.LabelDict structure VD = Identifier.VariableDict structure P = Prims 

 datatype value = VSymb of Identifier.variable | VInt of Int32.t 

 datatype shape = SAny | SDynamic of shape | SKnown of value * shape | SScalar | SUnknown 

 type t = shape VD.t 

 fun sMin ( _ , m ) = m  

 fun sMax ( m , _ ) = m  

 fun dbgPrint ( env , msg ) = if Config.debug andalso ( debugPass ( getConfig env ) ) then print msg else ( )  

 fun layoutShapeInfo ( s ) = let 

 fun value ( v ) = case v of VSymb v => Identifier.layoutVariable' ( v )| VInt i => Int32.layout ( i )  

 fun inner ( s ) = case s of SAny => [ L.str "..." ]| SDynamic n => ( L.str "." ) :: ( inner n )| SKnown ( v , n ) => ( value v ) :: ( inner n )| SScalar => [ ]| SUnknown => [ L.str "?" ]   in LU.bracketSeq ( inner s ) end  

 fun layoutShapeInfoP ( s ) = L.seq [ layoutShapeInfo ( sMax ( s ) ) , L.str "<" , layoutShapeInfo ( sMin ( s ) ) ]  

 fun shapeToString ( s ) = LU.toString ( layoutShapeInfoP ( s ) )  

 fun valueEq ( v1 , v2 ) = case ( v1 , v2 ) of ( VSymb var1 , VSymb var2 ) => var1 = var2| ( VInt i1 , VInt i2 ) => i1 = i2| _ => false  

 fun shpEq ( s1 , s2 ) = case ( s1 , s2 ) of ( SAny , SAny ) => true| ( SScalar , SScalar ) => true| ( SUnknown , SUnknown ) => true| ( SDynamic ( a ) , SDynamic ( b ) ) => shpEq ( a , b )| ( SKnown ( l1 , a ) , SKnown ( l2 , b ) ) => ( valueEq ( l1 , l2 ) ) andalso ( shpEq ( a , b ) )| _ => false  

 fun lub ( s1 , s2 ) = case ( s1 , s2 ) of ( SScalar , SScalar ) => SScalar| ( SDynamic ( a ) , SDynamic ( b ) ) => SDynamic ( lub ( a , b ) )| ( SDynamic ( a ) , SKnown ( _ , b ) ) => SDynamic ( lub ( a , b ) )| ( SKnown ( _ , a ) , SDynamic ( b ) ) => SDynamic ( lub ( a , b ) )| ( SKnown ( l1 , a ) , SKnown ( l2 , b ) ) => if ( valueEq ( l1 , l2 ) ) then SKnown ( l1 , lub ( a , b ) ) else SDynamic ( lub ( a , b ) )| ( other , SUnknown ) => other| ( SUnknown , other ) => other| _ => SAny  

 fun lubP ( ( a1 , a2 ) , ( b1 , b2 ) ) = ( lub ( a1 , b1 ) , lub ( a2 , b2 ) )  

 fun glb ( s1 , s2 ) = case ( s1 , s2 ) of ( SScalar , SScalar ) => SScalar| ( SDynamic a , SDynamic b ) => SDynamic ( glb ( a , b ) )| ( SDynamic a , SKnown ( i , b ) ) => SKnown ( i , glb ( a , b ) )| ( SKnown ( i , a ) , SDynamic b ) => SKnown ( i , glb ( a , b ) )| ( SKnown ( i1 , a ) , SKnown ( i2 , b ) ) => if ( valueEq ( i1 , i2 ) ) then SKnown ( i1 , glb ( a , b ) ) else SUnknown| ( other , SAny ) => other| ( SAny , other ) => other| _ => SUnknown  

 fun glbP ( ( a1 , a2 ) , ( b1 , b2 ) ) = ( glb ( a1 , b1 ) , glb ( a2 , b2 ) )  

 fun merge ( ( max1 , min1 ) , ( max2 , min2 ) ) = let 

 val max = glb ( max1 , max2 ) 

 val min = lub ( min1 , min2 )  in ( max , glb ( max , min ) ) end  

 fun unknownShape ( v ) = ( SAny , SUnknown )  

 fun constantToShape ( c ) = case c of M.CIntegral _ => ( SScalar , SScalar )| M.CFloat _ => ( SScalar , SScalar )| M.CDouble _ => ( SScalar , SScalar )| _ => ( SAny , SUnknown ) and simpleToShape ( st , s ) = case s of M.SConstant c => constantToShape ( c )| M.SVariable vv => st ( vv ) and typToShape ( t : M.typ ) = case t of M.TIntegral _ => ( SScalar , SUnknown )| M.TFloat => ( SScalar , SUnknown )| M.TDouble => ( SScalar , SUnknown )| M.TTuple { pok , fixed , array } => let 

 val b = pObjToShape pok 

 val arraytyp = SOME ( # 1 array ) 

 val tshp = Utils.Option.get ( Option.map ( arraytyp , typToShape ) , ( SAny , SUnknown ) ) 

 val a = ( SDynamic ( sMax ( tshp ) ) , SUnknown )  in merge ( a , b ) end| M.TPRef typ => typToShape typ| _ => ( SAny , SUnknown ) and pObjToShape ( pObj ) = case pObj of M.PokRat => ( SScalar , SUnknown )| M.PokFloat => ( SScalar , SUnknown )| M.PokDouble => ( SScalar , SUnknown )| _ => ( SAny , SUnknown )  

 fun nestShape ( len : M.simple , nest : shape ) = case len of M.SConstant ( M.CIntegral i ) => SKnown ( VInt ( Int32.fromIntInf ( IntArb.toIntInf i ) ) , nest )| M.SVariable v => SKnown ( VSymb v , nest )| _ => SDynamic nest  

 fun nestShapeP ( len : M.simple , ( s1 : shape , s2 : shape ) ) = ( nestShape ( len , s1 ) , nestShape ( len , s2 ) )  

 fun denestShape ( shp , def ) = case shp of SDynamic n => n| SKnown ( _ , n ) => n| _ => def  

 fun denestShapeP ( ( s1 , s2 ) , ( d1 , d2 ) ) = ( denestShape ( s1 , d1 ) , denestShape ( s2 , d2 ) )  

 fun deriveGlobal ( env , st , g ) = case g of M.GSimple s => simpleToShape ( st , s )| M.GRat _ => ( SScalar , SScalar )| _ => ( SAny , SAny )  

 fun derivePrim ( env , st , v , p , ops ) = case p of P.Prim _ => ( SScalar , SScalar )| _ => ( SAny , SAny )  

 fun deriveInstr ( env , st , dests , rhs ) = let 

 val some = Vector.new1 

 val none = Vector.new0 ( ) 

 val dest = fn () => Vector.sub ( dests , 0 ) 

 val res = case rhs of M.RhsSimple s => some ( dest ( ) , ( simpleToShape ( st , s ) ) )| M.RhsPrim { prim , createThunks , args } => ( case Utils.Vector.lookup ( dests , 0 ) of SOME vv => some ( vv , derivePrim ( env , st , vv , prim , args ) )| NONE => none )| M.RhsTuple { vtDesc , inits } => let 

 val vtd as M.VTD { pok , fixed , array } = vtDesc 

 val () = dbgPrint ( env , "TUPLE ->" ) 

 val pshp = pObjToShape ( pok ) 

 val () = dbgPrint ( env , "PSHP: " ^ shapeToString ( pshp ) ) 

 fun simpleToShape' s = simpleToShape ( st , s )  

 val tshp = ( Vector.fold ( inits , SUnknown , fn ( a , b ) => lub ( sMax ( simpleToShape' a ) , b ) ) , SUnknown ) 

 val () = dbgPrint ( env , "TSHP: " ^ shapeToString ( tshp ) )  in some ( dest ( ) , ( merge ( pshp , tshp ) ) ) end| _ => Vector.map ( dests , fn ( vv ) => ( vv , ( SAny , SAny ) ) )  in res end  

 fun deriveFunction ( env , st , _ , p , _ , _ ) = Vector.fromList ( List.duplicate ( p , fn () => ( SAny , SUnknown ) ) )  

 fun deriveBlock ( _ , dict , _ , args ) = Vector.map ( args , fn ( arg ) => simpleToShape ( dict , arg ) )  

 fun length ( si , v ) = case VD.lookup ( si , v ) of SOME ( SKnown ( l , _ ) ) => SOME l| _ => NONE  

 fun isScalar ( si , v ) = case VD.lookup ( si , v ) of SOME ( SScalar ) => true| _ => false  

 fun isArray ( si , v ) = case VD.lookup ( si , v ) of SOME ( SKnown _ ) => true| SOME ( SDynamic _ ) => true| _ => false  

 fun equalLength ( si , v1 , v2 ) = case ( length ( si , v1 ) , length ( si , v2 ) ) of ( SOME ( VInt i1 ) , SOME ( VInt i2 ) ) => i1 = i2| ( SOME ( VSymb s1 ) , SOME ( VSymb s2 ) ) => s1 = s2| _ => false  

 fun layout ( shp ) = VD.layout ( shp , fn ( k , s ) => L.seq [ I.layoutVariable' k , L.str " :: " , layoutShapeInfo s ] )  

 fun layout' ( shp ) = VD.layout ( shp , fn ( k , s ) => L.seq [ I.layoutVariable' k , L.str " :: " , layoutShapeInfoP s ] )  structure ShapeAnalysis = MilDataFlowAnalysisF ( 

 type env = env 

 type info = shape * shape structure MilCfg = MilCfg 

 val getConfig = getConfig 

 val passname = myPassname 

 val indent = indent + 2 

 val deriveConstant = fn ( _ , c ) => constantToShape ( c ) 

 val deriveInstr = deriveInstr 

 val deriveGlobal = deriveGlobal 

 val deriveFunction = deriveFunction 

 val deriveBlock = deriveBlock 

 val emptyInfo = unknownShape 

 val mergeInfo = fn ( _ , a , b ) => merge ( a , b ) 

 val coerceInfo = fn ( _ , _ , x ) => x 

 val equalInfo = fn ( _ , ( _ , a ) , ( _ , b ) ) => shpEq ( a , b ) 

 val layoutInfo = fn ( _ , a ) => layoutShapeInfoP ( a )  ) 

 fun program ( env , m ) = let 

 val sinfo = ShapeAnalysis.program ( env , m ) 

 val () = dbgPrint ( env , L.toString ( layout' sinfo ) )  in VD.map ( sinfo , fn ( _ , ( _ , min ) ) => min ) end  

 val debugs = [ debugPassD ] @ ShapeAnalysis.debugs  end 

