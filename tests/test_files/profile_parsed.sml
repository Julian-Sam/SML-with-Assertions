signature MIL_PROFILER = sig val debugs : Config.Debug.debug list type env type probability = Real.t type relFrequency = Real.t type absFrequency = IntInf.t structure CFG : sig type edgeRelFreq = ( relFrequency Mil.LD.t ) Mil.LD.t type blockRelFreq = relFrequency Mil.LD.t type cfgRelFreq = blockRelFreq * edgeRelFreq type edgeAbsFreq = ( absFrequency Mil.LD.t ) Mil.LD.t type blockAbsFreq = absFrequency Mil.LD.t type cfgAbsFreq = blockAbsFreq * edgeAbsFreq type edgeProb = ( probability Mil.LD.t ) Mil.LD.t val edgeProbabilities : env * MilCfg.t -> edgeProb val groupEdgeProbabilities : edgeProb list -> edgeProb val localFrequencies : env * MilCfg.t * edgeProb -> cfgRelFreq val groupLocalFrequencies : cfgRelFreq list -> cfgRelFreq  end ; structure CG : sig structure MilCG : MIL_CALL_GRAPH type funAbsFreq = absFrequency Mil.VD.t val funcFrequencies : env * Mil.t * CFG.blockRelFreq -> funAbsFreq val globalFrequencies : Mil.t * CFG.cfgRelFreq * funAbsFreq -> CFG.cfgAbsFreq  end ; val computeProfilingInfo : env * Mil.t -> CFG.edgeProb * CFG.cfgRelFreq * CFG.cfgAbsFreq  end functor MilProfilerF ( type env val getConfig : env -> Config.t val passname : string structure MilCG : MIL_CALL_GRAPH  ) :> MIL_PROFILER where type env = env = struct 

 type env = env structure PD = PassData structure ID = Identifier structure L = Layout 

 val mypassname = passname ^ ":MilProfiler" structure Chat = ChatF ( struct 

 type env = env 

 val extract = getConfig 

 val name = passname 

 val indent = 2  end ) 

 fun fail ( f , m ) = Fail.fail ( "profile.sml" , f , m )  

 type probability = Real.t 

 type absFrequency = IntInf.t 

 type relFrequency = Real.t 

 fun rel2absFrequency ( f ) = Real.toIntInf f  

 fun divRelFrequency ( f : relFrequency , d : int ) = ( f / real ( d ) )  

 fun divProbability ( p : probability , d : int ) = ( p / real ( d ) )  

 fun min ( a : relFrequency , b : relFrequency ) = if ( a < b ) then a else b  

 val maxCycProb = 0.99 structure Debug = struct 

 val ( debugPassD , debugPass ) = Config.Debug.mk ( mypassname , "debug the Mil Profiler module" ) 

 val printLevel : env * int * string -> unit = fn ( env , level , msg ) => if debugPass ( getConfig env ) andalso Config.debugLevel ( getConfig env , mypassname ) >= level then print ( mypassname ^ ": " ^ msg ) else ( ) 

 val print : env * string -> unit = fn ( env , msg ) => printLevel ( env , 0 , msg ) 

 val doLevel : env * int * ( unit -> unit ) -> unit = fn ( env , level , f ) => if debugPass ( getConfig env ) andalso Config.debugLevel ( getConfig env , mypassname ) >= level then f ( ) else ( ) 

 val printLayout : env * Layout.t -> unit = fn ( env , l ) => if debugPass ( getConfig env ) then LayoutUtils.printLayout ( L.seq [ L.str ( mypassname ^ ": " ) , l ] ) else ( )  end structure CFG = struct 

 type label = Mil.label 

 type edgeRelFreq = ( relFrequency Mil.LD.t ) Mil.LD.t 

 type blockRelFreq = relFrequency Mil.LD.t 

 type cfgRelFreq = blockRelFreq * edgeRelFreq 

 type edgeAbsFreq = ( absFrequency Mil.LD.t ) Mil.LD.t 

 type blockAbsFreq = absFrequency Mil.LD.t 

 type cfgAbsFreq = blockAbsFreq * edgeAbsFreq 

 type edgeProb = ( probability Mil.LD.t ) Mil.LD.t structure BranchProb = struct 

 datatype compare = CEq | CNe | CLt | CLe 

 datatype comparisonType = None | IntCmp of compare * Mil.operand * Mil.operand | Other 

 datatype info = Helpers of { cfg : MilCfg.t , getComparison : MilCfg.node -> comparisonType , getSuccessors : MilCfg.node -> ( MilCfg.node list ) * ( MilCfg.node list ) , getBoolSuccessors : MilCfg.node -> ( MilCfg.node * MilCfg.node ) option , postDominates : MilCfg.node * MilCfg.node -> bool , isReturnBlock : MilCfg.node -> bool , blockHasStore : MilCfg.node -> bool , blockHasCall : MilCfg.node -> bool , isLoopHeader : MilCfg.node -> bool , isLoopPreHeader : MilCfg.node -> bool , isLoopExit : MilCfg.node * MilCfg.node -> bool , isZero : Mil.constant -> bool } 

 datatype heuristics = LBH | CH | OH | LEH | RH | SH | LHH | GH 

 fun hitRate heuristic = case heuristic of LBH => 0.88| CH => 0.78| OH => 0.84| LEH => 0.80| RH => 0.72| SH => 0.55| LHH => 0.75| GH => 0.62  

 fun getComparison ( Helpers info , b ) = ( # getComparison info ) ( b )  

 fun getSuccessors ( Helpers info , b ) = ( # getSuccessors info ) ( b )  

 fun getBoolSuccessors ( Helpers info , b ) = ( # getBoolSuccessors info ) ( b )  

 fun postDominates ( Helpers info , s , b ) = ( # postDominates info ) ( s , b )  

 val notPostDominate = not o postDominates 

 fun isReturnBlock ( Helpers info , b ) = ( # isReturnBlock info ) ( b )  

 fun blockHasStore ( Helpers info , b ) = ( # blockHasStore info ) ( b )  

 fun blockHasCall ( Helpers info , b ) = ( # blockHasCall info ) ( b )  

 fun isLoopHeader ( Helpers info , b ) = ( # isLoopHeader info ) ( b )  

 fun isLoopPreHeader ( Helpers info , b ) = ( # isLoopPreHeader info ) ( b )  

 fun isLoopExit ( Helpers info , b , s ) = ( # isLoopExit info ) ( b , s )  

 fun isZero ( Helpers info , c ) = ( # isZero info ) ( c )  

 type probabilityHeuristic = info * MilCfg.node * MilCfg.node * MilCfg.node -> relFrequency option 

 val ReturnHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => case ( isReturnBlock ( info , s1 ) , isReturnBlock ( info , s2 ) ) of ( false , true ) => SOME ( hitRate RH )| ( true , false ) => SOME ( 1.0 - ( hitRate RH ) )| _ => NONE 

 val StoreHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => case ( blockHasStore ( info , s1 ) andalso notPostDominate ( info , s1 , n ) , blockHasStore ( info , s2 ) andalso notPostDominate ( info , s2 , n ) ) of ( false , true ) => SOME ( hitRate SH )| ( true , false ) => SOME ( 1.0 - ( hitRate SH ) )| _ => NONE 

 val CallHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => case ( blockHasCall ( info , s1 ) andalso notPostDominate ( info , s1 , n ) , blockHasCall ( info , s2 ) andalso notPostDominate ( info , s2 , n ) ) of ( false , true ) => SOME ( hitRate CH )| ( true , false ) => SOME ( 1.0 - ( hitRate CH ) )| _ => NONE 

 val LoopHeaderHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => case ( ( isLoopHeader ( info , s1 ) orelse isLoopPreHeader ( info , s1 ) ) andalso notPostDominate ( info , s1 , n ) , ( isLoopHeader ( info , s2 ) orelse isLoopPreHeader ( info , s2 ) ) andalso notPostDominate ( info , s2 , n ) ) of ( true , false ) => SOME ( hitRate LHH )| ( false , true ) => SOME ( 1.0 - ( hitRate LHH ) )| _ => NONE 

 val LoopExitHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => case ( isLoopHeader ( info , s1 ) orelse isLoopHeader ( info , s2 ) , isLoopExit ( info , n , s1 ) , isLoopExit ( info , n , s2 ) ) of ( false , false , true ) => SOME ( hitRate LEH )| ( false , true , false ) => SOME ( 1.0 - hitRate LEH )| _ => NONE 

 val GuardHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => fail ( "GuardHeuristic" , "Not implemented yet. EB: I'm not sure how this " ^ "heuristic applies to Mil." ) 

 val OpcodeHeuristic : probabilityHeuristic = fn ( info , n , s1 , s2 ) => case getComparison ( info , n ) of IntCmp ( CLt , Mil.SVariable _ , Mil.SConstant c ) => if ( isZero ( info , c ) ) then SOME ( 1.0 - hitRate LHH ) else NONE| IntCmp ( CLe , Mil.SVariable _ , Mil.SConstant c ) => if ( isZero ( info , c ) ) then SOME ( 1.0 - hitRate LHH ) else NONE| IntCmp ( CEq , Mil.SVariable _ , Mil.SConstant _ ) => SOME ( 1.0 - hitRate LHH )| IntCmp ( CEq , Mil.SConstant _ , Mil.SVariable _ ) => SOME ( 1.0 - hitRate LHH )| _ => NONE 

 datatype succType = NoSucc | UnreachableSucc of MilCfg.node list | BeAndOtherSucc of MilCfg.node list * MilCfg.node list | OnlyBackEdges of MilCfg.node list | BoolSuccsNoBe of MilCfg.node * MilCfg.node | NotBoolSucc of MilCfg.node list 

 fun callToExit ( b ) = false  

 fun classifySuccessors ( info as Helpers { cfg , ... } , b ) = let 

 val ( beSuccs , otherSuccs ) = getSuccessors ( info , b ) 

 val allSuccs = beSuccs @ otherSuccs 

 val m = List.length beSuccs 

 val n = List.length allSuccs  in if n = 0 then NoSucc else if ( callToExit ( b ) ) then UnreachableSucc allSuccs else if m > 0 andalso m < n then BeAndOtherSucc ( beSuccs , otherSuccs ) else if m > 0 andalso m = n then OnlyBackEdges beSuccs else if n <> 2 then NotBoolSucc otherSuccs else case getBoolSuccessors ( info , b ) of NONE => NotBoolSucc otherSuccs| SOME ( trueDst , falseDst ) => if MilCfg.compareNode ( trueDst , falseDst ) = EQUAL then NotBoolSucc [ trueDst , falseDst ] else BoolSuccsNoBe ( trueDst , falseDst ) end  

 val analyzeCFG : env * info * ( label * label * probability -> unit ) -> unit = fn ( env , info as Helpers { cfg , ... } , setEdgeProb' ) => let 

 fun setEdgeProb prob n1 n2 = let 

 val l1 = MilCfg.nodeGetLabel ( cfg , n1 ) 

 val l2 = MilCfg.nodeGetLabel ( cfg , n2 )  in case ( l1 , l2 ) of ( SOME src , SOME dst ) => setEdgeProb' ( src , dst , prob )| _ => ( ) end  

 fun analyzeBlock ( b : MilCfg.node ) = case classifySuccessors ( info , b ) of NoSucc => ( )| UnreachableSucc allSuccs => List.foreach ( allSuccs , setEdgeProb 0.0 b )| BeAndOtherSucc ( beSuccs , exitSuccs ) => let 

 val m = List.length ( beSuccs ) 

 val beProb = divProbability ( hitRate LBH , m ) 

 val nm = List.length ( exitSuccs ) 

 val exitProb = divProbability ( 1.0 - hitRate LBH , nm )  in ( List.foreach ( beSuccs , setEdgeProb beProb b ) ; List.foreach ( exitSuccs , setEdgeProb exitProb b ) ) end| OnlyBackEdges beSuccs => let 

 val m = List.length ( beSuccs ) 

 val beProb = divProbability ( 1.0 , m )  in List.foreach ( beSuccs , setEdgeProb beProb b ) end| NotBoolSucc otherSuccs => let 

 val m = List.length ( otherSuccs ) 

 val prob = divProbability ( 1.0 , m )  in List.foreach ( otherSuccs , setEdgeProb prob b ) end| BoolSuccsNoBe ( s1 , s2 ) => let 

 val heuristics : ( string * probabilityHeuristic ) list = [ ( "Return" , ReturnHeuristic ) , ( "Call" , CallHeuristic ) , ( "Loop Header" , LoopHeaderHeuristic ) , ( "Loop Exit" , LoopExitHeuristic ) ] 

 fun doHeuristic ( name , h ) = let 

 val probS1 = h ( info , b , s1 , s2 ) 

 fun printDebug () = case probS1 of SOME p => Debug.print ( env , " - " ^ name ^ " Heuristic Prob = " ^ ( Real.toString p ) ^ "\n" )| NONE => Debug.print ( env , " - " ^ name ^ " Heuristic does not apply\n" )  

 val () = Debug.doLevel ( env , 2 , printDebug )  in probS1 end  

 fun nodeLabelString ( n ) = case MilCfg.nodeGetLabel ( cfg , n ) of SOME l => ID.labelString l| NONE => "?"  

 fun printDebug () = Debug.print ( env , "Applying heuristics" ^ " to transfer at " ^ nodeLabelString ( b ) ^ " (S1 = " ^ nodeLabelString ( s1 ) ^ ", S2 = " ^ nodeLabelString ( s2 ) ^ ")\n" )  

 val () = Debug.doLevel ( env , 2 , printDebug ) 

 val bS1Probs = List.keepAllMap ( heuristics , doHeuristic ) 

 val bS1Prob = ref 0.5 

 val bS2Prob = ref 0.5 

 fun combineProbabilities ( bS1TakenProb ) = let 

 val d = ! bS1Prob * bS1TakenProb + ! bS2Prob * ( 1.0 - bS1TakenProb )  in ( bS1Prob := ! bS1Prob * bS1TakenProb / d ; bS2Prob := ! bS2Prob * ( 1.0 - bS1TakenProb ) / d ) end  

 fun hasLabel ( b ) = isSome ( MilCfg.nodeGetLabel ( cfg , b ) )  

 val () = if hasLabel ( s1 ) andalso hasLabel ( s2 ) then List.foreach ( bS1Probs , combineProbabilities ) else ( ) 

 val () = Debug.printLevel ( env , 2 , "Probability after " ^ "combining: " ^ "Blk -> S1 (true) = " ^ Real.toString ( ! bS1Prob ) ^ ", Blk -> S2 (false) = " ^ Real.toString ( ! bS2Prob ) ^ ".\n" )  in ( setEdgeProb ( ! bS1Prob ) b s1 ; setEdgeProb ( ! bS2Prob ) b s2 ) end   in List.foreach ( MilCfg.nodes cfg , analyzeBlock ) end  end 

 fun compareNodePair ( ( n1a , n1b ) , ( n2a , n2b ) ) = case MilCfg.compareNode ( n1a , n2a ) of EQUAL => MilCfg.compareNode ( n1b , n2b )| result => result  structure NodePairSet = SetF ( struct 

 type t = MilCfg.node * MilCfg.node 

 val compare = compareNodePair  end ) structure NodeSet = SetF ( struct 

 type t = MilCfg.node 

 val compare = MilCfg.compareNode  end ) structure CfgDominanceInfo = DominanceF ( 

 type node = MilCfg.node 

 val compare = MilCfg.compareNode  ) 

 fun buildCfgInfoHelpers ( env , cfg ) = let 

 val dt = MilCfg.getNodeDomTree ( cfg , MilCfg.entry cfg ) 

 val dom = CfgDominanceInfo.new dt 

 fun dominates ( n1 , n2 ) = CfgDominanceInfo.contains ( dom , n1 ) andalso CfgDominanceInfo.contains ( dom , n2 ) andalso CfgDominanceInfo.dominates ( dom , n1 , n2 )  

 val pdt = MilCfg.getNodePDomTree ( cfg , MilCfg.exit cfg ) 

 val pdom = CfgDominanceInfo.new pdt 

 fun postDominates ( n1 , n2 ) = CfgDominanceInfo.contains ( pdom , n1 ) andalso CfgDominanceInfo.contains ( pdom , n2 ) andalso CfgDominanceInfo.dominates ( pdom , n1 , n2 )  

 fun analyzeLoops ( allNodes : Mil.block ID.LabelDict.t ID.LabelDict.t , exitNodes : ID.LabelSet.t ID.LabelDict.t ) : ( NodeSet.t * NodePairSet.t ) = let 

 fun labelToNode ( l ) = MilCfg.labelGetNode ( cfg , l )  

 fun doLoopHeader ( h : Mil.label , myNodes : Mil.block ID.LabelDict.t ) = let 

 val exitNodes = case ID.LabelDict.lookup ( exitNodes , h ) of NONE => nil| SOME set => ID.LabelSet.toList set 

 fun doOne ( node : Mil.label ) = let 

 val n = labelToNode ( node ) 

 val succs = MilCfg.succ ( cfg , n ) 

 fun inLoop ( n ) = case MilCfg.nodeGetLabel ( cfg , n ) of NONE => false| SOME l => Mil.LD.contains ( myNodes , l )  

 val { no = exitSuccs , ... } = List.partition ( succs , inLoop )  in List.map ( exitSuccs , fn succ => ( n , succ ) ) end   in List.concat ( List.map ( exitNodes , doOne ) ) end  

 val exitEdgesLists = List.map ( ID.LabelDict.toList ( allNodes ) , doLoopHeader ) 

 val headersList = List.map ( ID.LabelDict.domain ( allNodes ) , labelToNode )  in ( NodeSet.fromList headersList , NodePairSet.fromList ( List.concat exitEdgesLists ) ) end  

 val lbdomtree = MilCfg.getLabelBlockDomTree ( cfg ) 

 val si = MilCfg.tGetSi cfg 

 val loops = MilLoop.build ( getConfig env , si , cfg , lbdomtree ) 

 val loops' = MilLoop.genAllNodes loops 

 val allNodes = MilLoop.allNodes loops' 

 val loops' = MilLoop.genExits loops' 

 val exitNodes = MilLoop.allExits loops' 

 val ( loopHeaders , loopExitEdges ) = analyzeLoops ( allNodes , exitNodes ) 

 fun isLoopExit ( n , s ) = NodePairSet.member ( loopExitEdges , ( n , s ) )  

 fun isLoopHeader ( lh ) = NodeSet.member ( loopHeaders , lh )  

 fun isReturnBlock ( n ) = let 

 val blk = valOf ( MilCfg.nodeGetBlock ( cfg , n ) )  in # exits ( MilUtils.Block.targets blk ) end  

 fun blockHasStore ( n ) : bool = fail ( "blockHasStore" , "Not implemented yet." )  

 fun backEdge ( n1 , n2 ) = dominates ( n2 , n1 )  

 fun getSuccessors ( n ) = let 

 val succs = MilCfg.succ ( cfg , n ) 

 val { no = otherSuccs , yes = backEdgeSuccs } = List.partition ( succs , fn s => backEdge ( n , s ) )  in ( backEdgeSuccs , otherSuccs ) end  

 fun getBoolSuccessors ( n ) = let 

 val b = valOf ( MilCfg.nodeGetBlock ( cfg , n ) )  in case MilUtils.Block.getBoolSuccessors ( b ) of NONE => NONE| SOME ( trueDst , falseDst ) => SOME ( MilCfg.labelGetNode ( cfg , trueDst ) , MilCfg.labelGetNode ( cfg , falseDst ) ) end  

 fun blockHasCall ( n ) = let 

 val blk = valOf ( MilCfg.nodeGetBlock ( cfg , n ) ) 

 val tfer = MilUtils.Block.transfer blk  in isSome ( MilUtils.Transfer.Dec.tInterProc tfer ) end  

 fun isLoopPreHeader ( b ) = case MilCfg.succ ( cfg , b ) of [ s ] => ( isLoopHeader ( s ) andalso dominates ( b , s ) )| _ => false  

 fun isZero ( c : Mil.constant ) = case c of Mil.CIntegral i => ( ( IntArb.toIntInf i ) = ( IntInf.fromInt 0 ) )| Mil.CFloat r => Real32.compare ( r , Real32.fromInt 0 ) = EQUAL| _ => false  

 fun getComparison ( n ) : BranchProb.comparisonType = fail ( "getComparison" , "Not implemented yet" )   in BranchProb.Helpers { cfg=cfg , getComparison=getComparison , getSuccessors=getSuccessors , getBoolSuccessors=getBoolSuccessors , postDominates=postDominates , isReturnBlock=isReturnBlock , blockHasStore=blockHasStore , blockHasCall=blockHasCall , isLoopHeader=isLoopHeader , isLoopPreHeader=isLoopPreHeader , isLoopExit=isLoopExit , isZero=isZero } end  

 val edgeProbabilities : env * MilCfg.t -> edgeRelFreq = fn ( env , cfg ) => let 

 val probDict = ref Mil.LD.empty 

 fun getDictionary ( l ) = case Mil.LD.lookup ( ! probDict , l ) of SOME dic => dic| NONE => let 

 val new = ref Mil.LD.empty 

 val () = probDict := Mil.LD.insert ( ! probDict , l , new )  in new end  

 fun addEdgeProb ( dict , dst , prob ) = dict := Mil.LD.insert ( ! dict , dst , prob )  

 fun setEdgeProb ( src , dst , prob ) = let 

 val dict = getDictionary ( src )  in case Mil.LD.lookup ( ! dict , dst ) of SOME _ => fail ( "setEdgeProb" , "Trying to set an existing probability." )| NONE => addEdgeProb ( dict , dst , prob ) end  

 val cfgInfoHelpers = buildCfgInfoHelpers ( env , cfg ) 

 val () = BranchProb.analyzeCFG ( env , cfgInfoHelpers , setEdgeProb )  in Mil.LD.map ( ! probDict , fn ( _ , d ) => ! d ) end 

 val groupEdgeProbabilities : edgeProb list -> edgeProb = fn ( edgeProbs ) => let 

 fun addEFreq _ = fail ( "groupEdgeProbabilities" , "Duplicated labes on edge probabilites." )  

 fun group ( eProb1 , eProb2 ) = Mil.LD.union ( eProb1 , eProb2 , addEFreq )   in List.fold ( edgeProbs , Mil.LD.empty , group ) end structure ACFG = struct structure Graph = ImpPolyLabeledGraph 

 datatype BlockInfo = BI of { block : label option , visited : bool ref , loopHeader : bool ref , bFreq : relFrequency ref } 

 datatype EdgeInfo = EI of { eFreq : relFrequency ref , beProb : relFrequency ref , prob : relFrequency ref , backEdge : bool ref } 

 type node = ( BlockInfo , EdgeInfo ) Graph.node 

 type edge = ( BlockInfo , EdgeInfo ) Graph.edge 

 type graph = ( BlockInfo , EdgeInfo ) Graph.t 

 type loopHeader = node 

 type loopForest = loopHeader Tree.t Vector.t 

 datatype t = ACFG of { graph : graph , entry : node , exit : node , loopForest : loopForest } 

 fun buildRefGetSet ( fld , getInfo ) = let 

 val get = op ! o fld o getInfo 

 val set = fn e => fn v => ( fld ( getInfo e ) ) := v  in ( get , set ) end  structure Node = struct 

 fun newInfo ( blockOpt : label option ) = BI { block=blockOpt , visited=ref false , loopHeader=ref false , bFreq=ref 0.0 }  

 fun getInfo ( n : ( BlockInfo , EdgeInfo ) Graph.node ) = ( case Graph.Node.getLabel ( n ) of ( BI info ) => info )  

 val getBlock = # block o getInfo 

 val ( getVisited , setVisited ) = buildRefGetSet ( # visited , getInfo ) 

 val ( isLoopHeader , setLoopHeader ) = buildRefGetSet ( # loopHeader , getInfo ) 

 val ( getBFreq , setBFreq ) = buildRefGetSet ( # bFreq , getInfo )  end structure Edge = struct 

 fun newInfo () = EI { eFreq=ref 0.0 , beProb=ref 0.0 , prob=ref 0.0 , backEdge=ref false }  

 fun getInfo e = ( case Graph.Edge.getLabel ( e ) of EI info => info )  

 val ( getEFreq , setEFreq ) = buildRefGetSet ( # eFreq , getInfo ) 

 val ( getBEProb , setBEProb ) = buildRefGetSet ( # beProb , getInfo ) 

 val ( getProb , setProb ) = buildRefGetSet ( # prob , getInfo ) 

 val ( isBackEdge , setBackEdge ) = buildRefGetSet ( # backEdge , getInfo )  end structure NodeDict = DictF ( struct 

 type t = node ; 

 val compare = Graph.Node.compare ;  end ) ; structure ACfgDominanceInfo = DominanceF ( 

 type node = node 

 val compare = Graph.Node.compare  ) 

 fun buildAnnotatedCfg ( d : env , cfg : MilCfg.t ) : t = let 

 fun mapFromMilCfgToACFG ( cfg ) = let 

 val cfg' : ( BlockInfo , EdgeInfo ) Graph.t = Graph.new ( ) 

 val blockDic = ref Mil.LD.empty 

 fun blockToNode' ( l ) = Mil.LD.lookup ( ! blockDic , l )  

 val nodeDic = ref MilCfg.NodeDict.empty 

 fun nodeToNode' ( n ) = MilCfg.NodeDict.lookup ( ! nodeDic , n )  

 fun doNode ( n : MilCfg.node ) = let 

 val block = MilCfg.nodeGetLabel ( cfg , n ) 

 val info = Node.newInfo ( block ) 

 val node' = Graph.newNode ( cfg' , info )  in ( nodeDic := MilCfg.NodeDict.insert ( ! nodeDic , n , node' ) ; if isSome ( block ) then blockDic := Mil.LD.insert ( ! blockDic , valOf ( block ) , node' ) else ( ) ) end  

 val nodes = MilCfg.nodes ( cfg ) 

 val () = List.foreach ( nodes , doNode ) 

 fun doEdge src dst = let 

 val src' = valOf ( nodeToNode' src ) 

 val dst' = valOf ( nodeToNode' dst ) 

 val info = EI { eFreq=ref 0.0 , beProb=ref 0.0 , prob=ref 0.0 , backEdge=ref false }  in ignore ( Graph.addEdge ( cfg' , src' , dst' , info ) ) end  

 fun doOne n = List.foreach ( MilCfg.succ ( cfg , n ) , doEdge n )  

 val () = List.foreach ( nodes , doOne )  in ( cfg' , blockToNode' , nodeToNode' ) end  

 val ( cfg' , blockToNode' , nodeToNode' ) = mapFromMilCfgToACFG ( cfg ) 

 val domTree = MilCfg.getLabelBlockDomTree cfg 

 val si = MilCfg.tGetSi cfg 

 val loops = MilLoop.build ( getConfig d , si , cfg , domTree ) 

 val loopForest : MilLoop.loop Tree.t vector = MilLoop.getLoops loops 

 fun getHeader ( MilLoop.L { header , ... } ) = valOf ( blockToNode' ( header ) )  

 val loopForest' : ( BlockInfo , EdgeInfo ) Graph.node Tree.t vector = Vector.map ( loopForest , fn t => Tree.map ( t , getHeader ) ) 

 fun setLoopHeader n = Node.setLoopHeader n true  

 val () = Vector.foreach ( loopForest' , fn t => Tree.foreachPre ( t , setLoopHeader ) ) 

 val entry' = valOf ( nodeToNode' ( MilCfg.entry cfg ) ) 

 val domTree = Graph.domTree ( cfg' , entry' ) 

 val dominance = ACfgDominanceInfo.new ( domTree ) 

 fun annotateBackEdge e = let 

 val src = Graph.Edge.from e 

 val dst = Graph.Edge.to e 

 val isBackEdg = ACfgDominanceInfo.contains ( dominance , dst ) andalso ACfgDominanceInfo.contains ( dominance , src ) andalso ACfgDominanceInfo.dominates ( dominance , dst , src ) 

 val () = if isBackEdg andalso not ( Node.isLoopHeader dst ) then fail ( "annotateBackEdge" , "Back edge points to a non loop header?" ) else ( )  in Edge.setBackEdge e isBackEdg end  

 val () = List.foreach ( Graph.edges ( cfg' ) , annotateBackEdge )  in ACFG { graph=cfg' , entry=entry' , exit=valOf ( nodeToNode' ( MilCfg.exit cfg ) ) , loopForest=loopForest' } end  

 fun addEdgesProbabilites ( acfg as ACFG { graph , entry , ... } , edgesProb : edgeProb ) : unit = let 

 fun annotateEdgesProb ( e ) = let 

 val srcBlk = Node.getBlock ( Graph.Edge.from ( e ) ) 

 val dstBlk = Node.getBlock ( Graph.Edge.to ( e ) ) 

 fun setProbabilities ( e , src , dst ) = let 

 fun notFound () = fail ( "setProbabilities" , "Could not find edge probability" )  

 val prob = case Mil.LD.lookup ( edgesProb , src ) of NONE => notFound ( )| SOME dic => case Mil.LD.lookup ( dic , dst ) of NONE => notFound ( )| SOME p => p  in ( Edge.setBEProb e prob ; Edge.setProb e prob ) end   in case ( srcBlk , dstBlk ) of ( SOME src , SOME dst ) => setProbabilities ( e , src , dst )| _ => ( ) end  

 val () = List.foreach ( Graph.edges ( graph ) , annotateEdgesProb ) 

 val e = case Graph.Node.outEdges entry of [ e ] => e| _ => fail ( "addEdgesProbabilites" , "Malformed Mil CFG. Entry node must have " ^ "a single outEdge" )  in ( Edge.setBEProb e 1.0 ; Edge.setProb e 1.0 ) end  

 fun fromInnerToOuterLoopsDo ( loopForest , doLoop ) = let 

 fun doLoopTree lt = Tree.foreachPost ( lt , doLoop )   in Vector.foreach ( loopForest , doLoopTree ) end  

 fun propagateFrequencies ( acfg as ACFG { graph = cfg , entry , loopForest , ... } ) : unit = let 

 fun propagateFreq ( b , head ) = Try.exec ( fn () => let 

 val () = Try.require ( not ( Node.getVisited b ) ) 

 fun computeBFreq ( b ) = let 

 fun checkPred ( e ) = let 

 val pred = Graph.Edge.from e  in Try.require ( Node.getVisited ( pred ) orelse Edge.isBackEdge ( e ) ) end  

 val () = List.foreach ( Graph.Node.inEdges b , checkPred ) 

 val cp = ref 0.0 

 val bFreq = ref 0.0 

 fun processPred ( e ) = if Edge.isBackEdge ( e ) then cp := ! cp + Edge.getBEProb ( e ) else bFreq := ! bFreq + Edge.getEFreq ( e )  

 val () = List.foreach ( Graph.Node.inEdges b , processPred ) 

 val cyclicProb = min ( ! cp , maxCycProb )  in ( ! bFreq / ( 1.0 - cyclicProb ) ) end  

 val () = if b = head then Node.setBFreq b 1.0 else Node.setBFreq b ( computeBFreq b ) 

 val () = Node.setVisited b true 

 fun processSucc edge = let 

 val succ = Graph.Edge.to edge 

 val fq = Edge.getProb ( edge ) * Node.getBFreq ( b ) 

 val () = Edge.setEFreq edge fq  in if succ = head then Edge.setBEProb edge fq else ( ) end  

 val () = List.foreach ( Graph.Node.outEdges b , processSucc ) 

 fun propagateToSucc edge = if not ( Edge.isBackEdge ( edge ) ) then propagateFreq ( Graph.Edge.to edge , head ) else ( )  

 val () = List.foreach ( Graph.Node.outEdges b , propagateToSucc )  in ( ) end )  

 fun markReachableAsNotVisited ( n ) = ( List.foreach ( Graph.nodes ( cfg ) , fn n => Node.setVisited n true ) ; List.foreach ( Graph.reachable ( cfg , n ) , fn n => Node.setVisited n false ) )  

 fun doLoop ( head ) = ( markReachableAsNotVisited ( head ) ; propagateFreq ( head , head ) )  

 val () = fromInnerToOuterLoopsDo ( loopForest , doLoop ) 

 val () = doLoop ( entry )  in ( ) end  

 fun getFrequencies ( acfg as ACFG { graph , ... } ) : cfgRelFreq = let 

 fun getBlockFreq ( n ) = case Node.getBlock n of SOME l => SOME ( l , Node.getBFreq n )| NONE => NONE  

 val blockFreqL = List.keepAllMap ( Graph.nodes graph , getBlockFreq ) 

 val blockFreqs = Mil.LD.fromList ( blockFreqL ) 

 fun buildOutDict ( l , n ) = let 

 fun doEdge ( e ) = case Node.getBlock ( Graph.Edge.to e ) of NONE => NONE| SOME dstBlock => let 

 val freq = Edge.getEFreq e  in SOME ( dstBlock , freq ) end  

 val freqList = List.keepAllMap ( Graph.Node.outEdges ( n ) , doEdge )  in if List.isEmpty ( freqList ) then NONE else SOME ( l , Mil.LD.fromList ( freqList ) ) end  

 fun getOutEdgsFreq ( n ) = case Node.getBlock n of NONE => NONE| SOME l => buildOutDict ( l , n )  

 val edgeFreqL = List.keepAllMap ( Graph.nodes graph , getOutEdgsFreq ) 

 val edgeFreqs = Mil.LD.fromList ( edgeFreqL )  in ( blockFreqs , edgeFreqs ) end   end 

 val localFrequencies : env * MilCfg.t * edgeProb -> cfgRelFreq = fn ( env , cfg , edgesProb ) => let 

 val acfg = ACFG.buildAnnotatedCfg ( env , cfg ) 

 val () = ACFG.addEdgesProbabilites ( acfg , edgesProb ) 

 val () = ACFG.propagateFrequencies ( acfg )  in ACFG.getFrequencies ( acfg ) end 

 val groupLocalFrequencies : cfgRelFreq list -> cfgRelFreq = fn ( cfgRelFreqs ) => let 

 fun addBFreq _ = fail ( "groupLocalFrequencies" , "Duplicated labels on block frequencies." )  

 fun addEFreq _ = fail ( "groupLocalFrequencies" , "Duplicated labels on edge frequencies." )  

 fun group ( ( bFreq1 , eFreq1 ) , ( bFreq2 , eFreq2 ) ) = let 

 val blkFreq = Mil.LD.union ( bFreq1 , bFreq2 , addBFreq ) 

 val edgFreq = Mil.LD.union ( eFreq1 , eFreq2 , addEFreq )  in ( blkFreq , edgFreq ) end   in List.fold ( cfgRelFreqs , ( Mil.LD.empty , Mil.LD.empty ) , group ) end  end ; structure CG = struct structure MilCG = MilCG 

 type funAbsFreq = absFrequency Mil.VD.t structure VS = ID.VariableSet structure L = Layout structure SCG = struct structure Graph = ImpPolyLabeledGraph 

 datatype nodeInfo = NI of { func : Mil.variable option , isLoopHeader : bool ref , cfreq : relFrequency ref , visited : bool ref } 

 datatype edgeInfo = EI of { backedge : bool ref , lfreq : relFrequency ref , gfreq : relFrequency ref , beProb : relFrequency ref } 

 type graph = ( nodeInfo , edgeInfo ) Graph.t 

 type node = ( nodeInfo , edgeInfo ) Graph.node 

 type edge = ( nodeInfo , edgeInfo ) Graph.edge 

 datatype t = SCG of { entry : node , funcNodes : node Mil.VD.t ref , graph : graph } structure Node = struct 

 fun newInfo ( f : Mil.variable option ) = NI { func=f , isLoopHeader=ref false , cfreq=ref 0.0 , visited=ref false }  

 fun foreachInEdge ( n , f ) = List.foreach ( Graph.Node.inEdges ( n ) , f )  

 fun foreachOutEdge ( n , f ) = List.foreach ( Graph.Node.outEdges ( n ) , f )  

 fun getInfo n = case Graph.Node.getLabel ( n ) of NI info => info  

 val getFunc = # func o getInfo 

 val getLoopHeader' = # isLoopHeader o getInfo 

 fun setLoopHeader ( n , v ) = ( getLoopHeader' n ) := v  

 fun isLoopHeader ( n ) = ! ( getLoopHeader' n )  

 val getVisited' = # visited o getInfo 

 fun setVisited ( n , v ) = ( getVisited' n ) := v  

 fun getVisited ( n ) = ! ( getVisited' n )  

 val getNotVisited = not o getVisited 

 val getCFreq' = # cfreq o getInfo 

 fun setCFreq ( n , freq ) = ( getCFreq' n ) := freq  

 fun getCFreq ( n ) = ! ( getCFreq' n )   end structure Edge = struct 

 val from = Graph.Edge.from 

 val to = Graph.Edge.to 

 fun getInfo e = case Graph.Edge.getLabel ( e ) of EI info => info  

 val isBackEdge' = # backedge o getInfo 

 fun isBackEdge ( e ) = ! ( isBackEdge' ( e ) )  

 fun setBackEdge ( e , v ) = ( isBackEdge' ( e ) ) := v  

 val getBEProb' = # beProb o getInfo 

 fun getBEProb ( e ) = ! ( getBEProb' e )  

 fun setBEProb ( e , prob ) = ( getBEProb' e ) := prob  

 val getGFreq' = # gfreq o getInfo 

 fun setGFreq ( e , freq ) = ( getGFreq' e ) := freq  

 fun getGFreq ( e ) = ! ( getGFreq' e )  

 val getLFreq' = # lfreq o getInfo 

 fun setLFreq ( e , freq ) = ( getLFreq' e ) := freq  

 fun getLFreq ( e ) = ! ( getLFreq' e )   end 

 fun new () = let 

 val g = Graph.new ( ) 

 val entry = Graph.newNode ( g , Node.newInfo ( NONE ) )  in SCG { entry=entry , funcNodes=ref Mil.VD.empty , graph=g } end  

 val layout : t * ( Mil.variable -> Layout.t ) -> Layout.t = fn ( SCG { entry , graph , ... } , layoutVar ) => let 

 fun layoutFunc f = case f of NONE => L.str "?? (No func)"| SOME v => layoutVar v  

 fun layoutNode ( n : ( nodeInfo , edgeInfo ) Graph.node ) = if Graph.Node.equal ( n , entry ) then L.str "Entry Node" else layoutFunc ( Node.getFunc n )  

 fun layoutNodeEdges n = let 

 fun layoutEdge e = let 

 val tgt : ( nodeInfo , edgeInfo ) Graph.node = Edge.to e 

 val be = if Edge.isBackEdge e then L.str " (BackEdge)" else L.str ""  in L.seq [ L.str "   -> " , layoutNode tgt , be , L.str " [Freq: " , Real.layout ( Edge.getGFreq ( e ) ) , L.str "]\n" ] end  

 val edges = List.map ( Graph.Node.outEdges n , layoutEdge ) 

 val loopHeader = if Node.isLoopHeader n then L.str "(Loop Header)" else L.str "" 

 val node = L.seq [ layoutNode n , L.str " [Freq: " , Real.layout ( Node.getCFreq n ) , L.str "]\n" , loopHeader ]  in L.align ( node :: edges ) end   in L.align ( List.map ( Graph.nodes ( graph ) , layoutNodeEdges ) ) end 

 fun getEntryNode ( SCG g ) = # entry g  

 fun getFuncNodesDict ( SCG g ) = # funcNodes g  

 fun getGraph ( SCG g ) = # graph g  

 fun containsNode ( scg , n' ) = List.contains ( Graph.nodes ( getGraph scg ) , n' , Graph.Node.equal )  

 fun newFuncNode ( scg : t , f : Mil.variable ) = let 

 val node = Graph.newNode ( getGraph scg , Node.newInfo ( SOME f ) ) 

 val funcNodesDic = getFuncNodesDict scg 

 val () = funcNodesDic := Mil.VD.insert ( ! funcNodesDic , f , node )  in node end  

 fun hasFuncNode ( SCG g , func ) = Mil.VD.contains ( ! ( # funcNodes g ) , func )  

 fun lookupFuncNode ( SCG g , func : VS.element ) = Mil.VD.lookup ( ! ( # funcNodes g ) , func )  

 fun reversePostOrdering ( SCG g : t , root : node ) : node list = Graph.revPostOrderDfs ( # graph g , root )  

 fun foreachEdge ( SCG g , doIt ) = List.foreach ( Graph.edges ( # graph g ) , fn ( e ) => doIt ( e ) )  

 fun hasEdge ( SCG g , srcN , tgtN ) = not ( List.isEmpty ( Graph.Node.getEdges ( srcN , tgtN ) ) )  

 fun newEdge ( SCG g , srcN , dstN ) = let 

 val info = EI { backedge=ref false , lfreq=ref 0.0 , gfreq=ref 0.0 , beProb=ref 0.0 }  in Graph.addEdge ( # graph g , srcN , dstN , info ) end  structure CGDominanceInfo = DominanceF ( 

 type node = node 

 val compare = Graph.Node.compare  ) 

 fun getDominanceInfo ( SCG scg , entryNode ) : CGDominanceInfo.t = CGDominanceInfo.new ( Graph.domTree ( # graph scg , entryNode ) )  

 fun annotateBackEdgesAndLoopHeaders ( d , scg as SCG { entry , ... } ) = let 

 val domTree = getDominanceInfo ( scg , entry ) 

 fun annotateEdge ( e ) = let 

 val src = Edge.from ( e ) 

 val dst = Edge.to ( e ) 

 fun printNode n = case ( Node.getFunc n ) of NONE => "Entry node"| SOME v => ID.variableString' v   in if not ( CGDominanceInfo.contains ( domTree , src ) ) then ( Edge.setBackEdge ( e , true ) ; Chat.warn0 ( d , "Function Node " ^ ( printNode src ) ^ " is not present in dominator tree. " ^ "No path from entry to non-escaping P " ^ " function?\n" ) ) else if not ( CGDominanceInfo.contains ( domTree , dst ) ) then ( Edge.setBackEdge ( e , true ) ; Chat.warn0 ( d , "Function Node " ^ ( printNode dst ) ^ " is not present in dominator tree. " ^ "No path from entry to non-escaping P " ^ "function?\n" ) ) else if CGDominanceInfo.dominates ( domTree , dst , src ) then ( Edge.setBackEdge ( e , true ) ; Node.setLoopHeader ( dst , true ) ) else Edge.setBackEdge ( e , false ) end   in foreachEdge ( scg , annotateEdge ) end  

 fun build ( d , p : Mil.t , localBlockFreq ) : t = let 

 val st = MilUtils.Program.symbolTable p 

 val si = Identifier.SymbolInfo.SiTable st 

 val cg = MilCG.program ( getConfig d , si , p ) 

 val MilCG.CG { funs , calls , callMap } = cg 

 val scg = new ( ) 

 fun initEdgeFreq ( e , freq ) = ( Edge.setLFreq ( e , freq ) ; Edge.setBEProb ( e , freq ) )  

 val Mil.P { entry = mainFun , ... } = p 

 fun processFun ( f , MilCG.FI info ) = let 

 val node = if hasFuncNode ( scg , f ) then fail ( "processFun" , "Mil program has a duplicated function." ) else newFuncNode ( scg , f ) 

 val entry = getEntryNode ( scg ) 

 fun equalFunc ( f1 , f2 ) = MilUtils.Compare.variable ( f1 , f2 ) = EQUAL   in if # unknownCallers info orelse equalFunc ( f , mainFun ) then initEdgeFreq ( newEdge ( scg , entry , node ) , 1.0 ) else ( ) end  

 val () = Mil.VD.foreach ( funs , processFun ) 

 fun requireNode ( f ) = case lookupFuncNode ( scg , f ) of SOME n => n| NONE => fail ( "requireNode" , "Could not find " ^ "required node." )  

 fun processCall ( callBlock : CFG.label , MilCG.CI ci ) = let 

 val inFun = Option.valOf ( Identifier.LabelDict.lookup ( callMap , callBlock ) ) 

 val srcFun : VS.element = inFun 

 val srcNode = requireNode ( srcFun ) 

 val targets = VS.toList ( # knownCallees ci ) 

 val nTargets = List.length ( targets ) 

 val callFreq = case Mil.LD.lookup ( localBlockFreq , callBlock ) of SOME freq => freq| NONE => 0.0 

 val sharedFreq = if nTargets > 0 then divRelFrequency ( callFreq , nTargets ) else 0.0 

 fun processEdge ( dstFun ) = let 

 val dstNode = requireNode ( dstFun ) 

 val e = case Graph.Node.getEdges ( srcNode , dstNode ) of nil => newEdge ( scg , srcNode , dstNode )| [ e ] => e| _ => fail ( "processEdge" , "found multiple edges in SCG." ) 

 val newLFreq = Edge.getLFreq ( e ) + sharedFreq  in initEdgeFreq ( e , newLFreq ) end   in List.foreach ( targets , processEdge ) end  

 val () = Mil.LD.foreach ( calls : MilCG.callInfo ID.LabelDict.t , processCall )  in scg end  

 fun propagateFrequencies ( scg ) = let 

 fun propagateCallFreq ( f : node , head : node , final : bool ) : unit = Try.exec ( fn () => let 

 val () = Try.require ( Node.getNotVisited f ) 

 fun srcVisited ( e ) = Node.getVisited ( Edge.from e )  

 fun chkCondition ( e ) = Try.require ( srcVisited ( e ) orelse Edge.isBackEdge ( e ) )  

 val () = Node.foreachInEdge ( f , chkCondition ) 

 val cfreq' = if ( f = head ) then ref 1.0 else ref 0.0 

 val cyclicProb = ref 0.0 

 fun analyzePred ( e ) = if final andalso Edge.isBackEdge ( e ) then cyclicProb := ! cyclicProb + Edge.getBEProb ( e ) else cfreq' := ! cfreq' + Edge.getGFreq ( e )  

 val () = Node.foreachInEdge ( f , analyzePred ) 

 val cyclicProb = min ( ! cyclicProb , maxCycProb ) 

 val () = Node.setCFreq ( f , ! cfreq' / ( 1.0 - cyclicProb ) ) 

 val () = Node.setVisited ( f , true ) 

 fun doOne ( e ) = let 

 val edgGFreq = Edge.getLFreq ( e ) * Node.getCFreq ( f ) 

 val () = Edge.setGFreq ( e , edgGFreq )  in if f = head andalso not final then Edge.setBEProb ( e , edgGFreq ) else ( ) end  

 val () = Node.foreachOutEdge ( f , doOne ) 

 fun doRecursive ( e ) = if not ( Edge.isBackEdge ( e ) ) then propagateCallFreq ( Edge.to ( e ) , head , final ) else ( )   in Node.foreachOutEdge ( f , doRecursive ) end )  

 fun markReachableAsNotVisited ( SCG scg , n ) : unit = ( List.foreach ( Graph.nodes ( # graph scg ) , fn n => Node.setVisited ( n , true ) ) ; List.foreach ( Graph.reachable ( # graph scg , n ) , fn n => Node.setVisited ( n , false ) ) )  

 fun propagateCallFreqForLoop ( f ) = if ( Node.isLoopHeader ( f ) ) then ( markReachableAsNotVisited ( scg , f ) ; propagateCallFreq ( f , f , false ) ) else ( )  

 val entryFunc = getEntryNode ( scg ) 

 val reverseDFO = reversePostOrdering ( scg , entryFunc ) 

 val () = List.foreach ( reverseDFO , propagateCallFreqForLoop ) 

 val () = markReachableAsNotVisited ( scg , entryFunc ) 

 val () = propagateCallFreq ( entryFunc , entryFunc , true )  in ( ) end   end 

 val funcFrequencies : env * Mil.t * CFG.blockRelFreq -> funAbsFreq = fn ( d , p , localBlockFreq ) => let 

 val scg = SCG.build ( d , p , localBlockFreq ) 

 val () = SCG.annotateBackEdgesAndLoopHeaders ( d , scg ) 

 val () = SCG.propagateFrequencies ( scg ) 

 fun convertOne ( f , n ) = rel2absFrequency ( SCG.Node.getCFreq ( n ) )   in Mil.VD.map ( ! ( SCG.getFuncNodesDict ( scg ) ) , convertOne ) end 

 fun mapBlockToFunFreq ( p as Mil.P { globals , ... } : Mil.t , cgFunAbsFreq : absFrequency Mil.VD.t ) = let 

 val funcFreq = ref Mil.LD.empty 

 fun getRelFuncFreq ( f ) = case Mil.VD.lookup ( cgFunAbsFreq , f ) of SOME freq => Real.fromIntInf ( freq )| _ => 0.0  

 fun updateFunctionFreqs ( f , Mil.F { body = Mil.CB { entry , blocks } , ... } ) = let 

 val relFuncFreq = getRelFuncFreq ( f ) 

 fun updateBlockFuncFreq ( l , _ ) = funcFreq := Mil.LD.insert ( ! funcFreq , l , relFuncFreq )   in Mil.LD.foreach ( blocks , updateBlockFuncFreq ) end  

 fun updateFreqs ( f , g ) = case g of Mil.GCode code => updateFunctionFreqs ( f , code )| _ => ( )  

 val cfgsBBFreq = Mil.VD.foreach ( globals , updateFreqs )  in ! funcFreq end  

 val globalFrequencies : Mil.t * CFG.cfgRelFreq * funAbsFreq -> CFG.cfgAbsFreq = fn ( p as Mil.P { globals , ... } , ( blkRelFreq , edgRelFreq ) , funAbsFreq ) => let 

 val funcFreq = mapBlockToFunFreq ( p , funAbsFreq ) 

 fun getFuncFreq ( blk ) = case Mil.LD.lookup ( funcFreq , blk ) of NONE => 0.0| SOME f => f  

 fun scaleByFunFreq ( blk , freq ) = rel2absFrequency ( getFuncFreq ( blk ) * freq )  

 val blockFreq = Mil.LD.map ( blkRelFreq , scaleByFunFreq ) 

 fun scaleByFunFreq ( src , dict ) = let 

 val scale = getFuncFreq ( src )  in Mil.LD.map ( dict , fn ( dst , freq ) => rel2absFrequency ( scale * freq ) ) end  

 val edgeFreq = Mil.LD.map ( edgRelFreq , scaleByFunFreq )  in ( blockFreq , edgeFreq ) end  end ; 

 val computeProfilingInfo : env * Mil.t -> CFG.edgeProb * CFG.cfgRelFreq * CFG.cfgAbsFreq = fn ( d , p as Mil.P { globals , ... } ) => let 

 fun getCodeProfile ( code as Mil.F { body , ... } ) = let 

 val st = MilUtils.Program.symbolTable p 

 val si = Identifier.SymbolInfo.SiTable st 

 val cfg = MilCfg.build ( getConfig d , si , body ) 

 val edgProb = CFG.edgeProbabilities ( d , cfg ) 

 val localFreq = CFG.localFrequencies ( d , cfg , edgProb )  in ( edgProb , localFreq ) end  

 fun getFuncProfile ( _ , g ) = case g of Mil.GCode code => SOME ( getCodeProfile ( code ) )| _ => NONE  

 val cfgsProfile = Mil.VD.range ( Mil.VD.keepAllMap ( globals , getFuncProfile ) ) 

 val ( cfgsEdgProbs , cfgsRelFreqs ) = List.unzip ( cfgsProfile ) 

 val milEdgProbs = CFG.groupEdgeProbabilities ( cfgsEdgProbs ) 

 val milRelFreqs = CFG.groupLocalFrequencies ( cfgsRelFreqs ) 

 val callGraphFreq = CG.funcFrequencies ( d , p , # 1 milRelFreqs ) 

 val milAbsFreqs = CG.globalFrequencies ( p , milRelFreqs , callGraphFreq )  in ( milEdgProbs , milRelFreqs , milAbsFreqs ) end 

 val debugs = [ Debug.debugPassD ]  end 

