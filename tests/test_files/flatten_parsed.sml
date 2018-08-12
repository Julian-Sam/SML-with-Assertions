structure MilRepFlattenOptimization :> MIL_REP_OPTIMIZATION = struct 

 val passname = "MilFlatten" 

 val description = "Flattening optimization" 

 val reconstructTypes = false 

 val fail = fn ( fname , msg ) => Fail.fail ( "flatten.sml" , fname , msg ) structure M = Mil structure P = Mil.Prims structure MU = MilUtils structure MSTM = MU.SymbolTableManager structure MFV = MilFreeVars structure VS = Mil.VS structure VD = Mil.VD structure PD = PassData structure SS = StringSet structure MT = MilType structure MTT = MT.Type structure FG = MilRepFlowGraph structure MRS = MilRepSummary structure ID = IntDict structure I = Identifier structure STM = I.Manager 

 val <@ = Try.<@ structure Chat = ChatF ( struct 

 type env = PD.t 

 val extract = PD.getConfig 

 val name = passname 

 val indent = 2  end ) 

 val mkDebug = fn ( tag , description ) => PD.mkDebug ( passname ^ ":" ^ tag , description ) 

 val ( debugPassD , debugPass ) = mkDebug ( "debug" , "Debug flatten according to debug level" ) 

 val mkLevelDebug = fn ( tag , description , level ) => PD.mkLevelDebug ( passname , passname ^ ":" ^ tag , description , level , debugPass ) 

 val ( showFlatteningD , showFlattening ) = mkLevelDebug ( "show-flattening" , "Show flattening analysis" , 1 ) 

 val debug = fn ( pd , i ) => debugPass pd andalso ( Config.debugLevel ( PD.getConfig pd , passname ) >= i ) 

 val debugs = [ debugPassD , showFlatteningD ] 

 val features = [ ] structure Click = struct 

 val stats = [ ] 

 val { stats , click = flatten } = PD.clicker { stats=stats , passname=passname , name="Flatten" , desc="Argument tuples flattened" }  end 

 val stats = Click.stats structure Flatten = struct structure TLat = LatticeFn ( struct 

 type element = Config.t * Mil.typ 

 val lub = fn ( ( config , t1 ) , ( _ , t2 ) ) => SOME ( config , MTT.lub ( config , t1 , t2 ) )  end ) structure Lat = LatticeVectorLatticeFn ( structure Lattice = TLat  ) 

 datatype 'data state = S of { summary : MRS.summary , flowgraph : 'data FG.t } 

 datatype env = E of { pd : PD.t } 

 val getSummary = fn ( S { summary , ... } ) => summary 

 val getFlowgraph = fn ( S { flowgraph , ... } ) => flowgraph 

 val getPd = fn ( E { pd , ... } ) => pd 

 val getConfig = PD.getConfig o getPd 

 val typOfVariable = fn ( ( s , e ) , v ) => let 

 val summary = getSummary s 

 val t = MRS.variableTyp ( summary , v )  in t end 

 val flattenTuple = fn ( s , e , noFlatten , dests , { mdDesc , inits } ) => let 

 val summary = getSummary s 

 val config = getConfig e 

 val flattened = Try.try ( fn () => let 

 val dest = Try.V.singleton dests 

 val vs = Vector.map ( inits , Try.<@ MU.Simple.Dec.sVariable ) 

 val ts = Vector.map ( vs , fn v => TLat.elt ( config , MRS.variableTyp ( summary , v ) ) ) 

 val elt = Lat.elt ts 

 val node = MRS.variableNode ( summary , dest ) 

 val () = FG.add ( getFlowgraph s , node , elt )  in ( ) end ) 

 val () = if isSome flattened then ( ) else noFlatten ( )  in ( ) end structure Analyze1 = MilAnalyseF ( struct 

 type state = Lat.t state 

 type env = env 

 val config = getConfig 

 val indent = 2 

 val externBind = NONE 

 val variableBind = NONE 

 val labelBind = NONE 

 val variableUse = NONE 

 val analyseJump = NONE 

 val analyseCut = NONE 

 val analyseConstant = NONE 

 val analyseInstruction' = fn ( s , e , M.I { dests , n , rhs } ) => let 

 val summary = getSummary s 

 val noFlatten = fn () => let 

 val () = if debug ( getPd e , 1 ) then LayoutUtils.printLayout ( Layout.seq [ Layout.str "Variables cannot be flattened: " , Vector.layout ( Identifier.layoutVariable' ) dests ] ) else ( ) 

 val mark = fn v => FG.add ( getFlowgraph s , MRS.variableNode ( summary , v ) , Lat.top )  in Vector.foreach ( dests , mark ) end 

 val () = ( case rhs of M.RhsSimple s => ( case s of M.SVariable _ => ( )| _ => noFlatten ( ) )| M.RhsPrim p => ( case p of { prim = P.Prim P.PCondMov , args = args , ... } => ( case Try.try ( fn () => Try.V.tripleton args ) of SOME ( _ , M.SVariable _ , M.SVariable _ ) => ( )| _ => noFlatten ( ) )| _ => noFlatten ( ) )| M.RhsTuple r => flattenTuple ( s , e , noFlatten , dests , r )| M.RhsTupleSub _ => noFlatten ( )| M.RhsTupleSet _ => noFlatten ( )| M.RhsTupleCAS _ => noFlatten ( )| M.RhsTupleWait _ => noFlatten ( )| M.RhsTupleInited _ => noFlatten ( )| M.RhsIdxGet _ => noFlatten ( )| M.RhsCont _ => noFlatten ( )| M.RhsThunkMk _ => noFlatten ( )| M.RhsThunkInit _ => noFlatten ( )| M.RhsThunkGetFv _ => noFlatten ( )| M.RhsThunkValue _ => noFlatten ( )| M.RhsThunkGetValue _ => noFlatten ( )| M.RhsThunkSpawn _ => noFlatten ( )| M.RhsClosureMk _ => noFlatten ( )| M.RhsClosureInit _ => noFlatten ( )| M.RhsClosureGetFv _ => noFlatten ( )| M.RhsPSetNew _ => noFlatten ( )| M.RhsPSetGet _ => noFlatten ( )| M.RhsPSetCond _ => noFlatten ( )| M.RhsPSetQuery _ => noFlatten ( )| M.RhsEnum _ => noFlatten ( )| M.RhsSum _ => noFlatten ( )| M.RhsSumProj _ => noFlatten ( )| M.RhsSumGetTag _ => noFlatten ( ) )  in e end 

 val analyseInstruction = SOME analyseInstruction' 

 val analyseTransfer' = fn ( s , e , lo , t ) => let 

 val summary = getSummary s 

 val noFlatten = fn ret => ( case ret of M.RTail _ => ( )| M.RNormal { rets , ... } => let 

 val f = fn v => FG.add ( getFlowgraph s , MRS.variableNode ( summary , v ) , Lat.top )  in Vector.foreach ( rets , f ) end ) 

 val () = ( case t of M.TGoto _ => ( )| M.TCase _ => ( )| M.TInterProc { ret , ... } => noFlatten ret| M.TReturn _ => ( )| M.TCut _ => ( )| M.THalt _ => ( ) )  in e end 

 val analyseTransfer = SOME analyseTransfer' 

 val analyseBlock = NONE 

 val analyseGlobal' = fn ( s , e , v , g ) => let 

 val summary = getSummary s 

 val noFlatten = fn () => let 

 val () = if debug ( getPd e , 1 ) then LayoutUtils.printLayout ( Layout.seq [ Layout.str "Global cannot be flattened: " , Identifier.layoutVariable' v ] ) else ( ) 

 val () = FG.add ( getFlowgraph s , MRS.variableNode ( summary , v ) , Lat.top )  in ( ) end 

 val () = ( case g of M.GCode _ => ( case MilRepSummary.iInfo ( summary , MU.Id.G v ) of MilRepBase.IiCode { returns , ... } => Vector.foreach ( returns , fn n => FG.add ( getFlowgraph s , n , Lat.top ) )| _ => fail ( "analyseGlobal'" , "Bad function information" ) )| M.GErrorVal t => noFlatten ( )| M.GIdx _ => noFlatten ( )| M.GTuple r => flattenTuple ( s , e , noFlatten , Vector.new1 v , r )| M.GRat _ => noFlatten ( )| M.GInteger _ => noFlatten ( )| M.GCString _ => noFlatten ( )| M.GThunkValue _ => noFlatten ( )| M.GSimple ( M.SVariable _ ) => ( )| M.GSimple _ => noFlatten ( )| M.GClosure _ => noFlatten ( )| M.GSum _ => noFlatten ( )| M.GPSet _ => noFlatten ( ) )  in e end 

 val analyseGlobal = SOME analyseGlobal'  end ) 

 val forward1 = fn ( pd , summary , p ) => let 

 val veq = fn ( v1 , v2 ) => Vector.equals ( v1 , v2 , TLat.equal ( fn ( ( _ , t1 ) , ( _ , t2 ) ) => MTT.equal ( t1 , t2 ) ) ) 

 val fgF1 = FG.build { pd=pd , forward=true , summary=summary , uDefInit=SOME Lat.top , uUseInit=SOME Lat.top , initialize=fn n => Lat.bot , merge=Lat.join , equal=Lat.equal veq } 

 val state = S { summary=summary , flowgraph=fgF1 } 

 val env = E { pd=pd } 

 val () = Analyze1.analyseProgram ( state , env , p ) 

 val () = FG.propagate fgF1  in fgF1 end structure Analyze2 = MilAnalyseF ( struct 

 type state = bool state 

 type env = env 

 val config = getConfig 

 val indent = 2 

 val externBind = NONE 

 val variableBind = NONE 

 val labelBind = NONE 

 val variableUse = NONE 

 val analyseJump = NONE 

 val analyseCut = NONE 

 val analyseConstant = NONE 

 val noFlatten = fn ( s , e , v ) => let 

 val summary = getSummary s 

 val () = if debug ( getPd e , 1 ) then LayoutUtils.printLayout ( Layout.seq [ Layout.str "Variable use cannot be flattened: " , Identifier.layoutVariable' v ] ) else ( ) 

 val () = FG.add ( getFlowgraph s , MRS.variableNode ( summary , v ) , false )  in ( ) end 

 val noFlattenO = fn ( s , e , c ) => let 

 val v = ( case c of M.SVariable v => v| _ => fail ( "flatten" , "Not in named form" ) )  in noFlatten ( s , e , v ) end 

 val analyseInstruction' = fn ( s , e , M.I { dests , n , rhs } ) => let 

 val noFlatten = fn v => noFlatten ( s , e , v ) 

 val noFlattenO = fn c => noFlattenO ( s , e , c ) 

 val noFlattenRhs = fn rhs => VS.foreach ( MFV.rhs ( getConfig e , rhs ) , noFlatten ) 

 val () = ( case rhs of M.RhsSimple ( M.SVariable _ ) => ( )| M.RhsPrim p => ( case p of { prim = P.Prim P.PCondMov , args = args , ... } => ( case Try.try ( fn () => Try.V.tripleton args ) of SOME ( _ , M.SVariable _ , M.SVariable _ ) => ( )| _ => noFlattenRhs rhs )| _ => noFlattenRhs rhs )| M.RhsTupleSub tf => if MU.TupleField.isFixed tf then ( ) else noFlattenRhs rhs| _ => noFlattenRhs rhs )  in e end 

 val analyseInstruction = SOME analyseInstruction' 

 val analyseTransfer' = fn ( s , e , lo , t ) => let 

 val config = getConfig e 

 val noFlatten = fn v => noFlatten ( s , e , v ) 

 val noFlattenO = fn c => noFlattenO ( s , e , c ) 

 val () = ( case t of M.TGoto _ => ( )| M.TCase { on , ... } => noFlattenO on| M.TInterProc { callee , ... } => ( case callee of M.IpCall { call , ... } => VS.foreach ( MFV.call ( config , call ) , noFlatten )| M.IpEval { eval , ... } => VS.foreach ( MFV.eval ( config , eval ) , noFlatten ) )| M.TReturn opers => Vector.foreach ( opers , noFlattenO )| M.TCut { cont , args , ... } => let 

 val () = Vector.foreach ( args , noFlattenO ) 

 val () = noFlatten cont  in ( ) end| M.THalt opnd => noFlattenO opnd )  in e end 

 val analyseTransfer = SOME analyseTransfer' 

 val analyseBlock = NONE 

 val analyseGlobal' = fn ( s , e , v , g ) => let 

 val noFlatten = fn v => noFlatten ( s , e , v ) 

 val noFlattenO = fn c => noFlattenO ( s , e , c ) 

 val () = ( case g of M.GSimple ( M.SVariable _ ) => ( )| M.GCode _ => ( )| _ => VS.foreach ( MFV.global ( getConfig e , v , g ) , noFlatten ) )  in e end 

 val analyseGlobal = SOME analyseGlobal'  end ) 

 val flattenOk = fn ( config , fgF1 , fgB , n ) => let 

 val traceable = fn tl => ( case TLat.get tl of NONE => false| SOME ( _ , t ) => isSome ( MU.Typ.traceability ( config , t ) ) ) 

 val ok = ( case Lat.get ( FG.query ( fgF1 , n ) ) of NONE => false| SOME vs => Vector.forall ( vs , traceable ) ) 

 val () = if ok then ( ) else FG.add ( fgB , n , false )  in ( ) end 

 val backward = fn ( pd , summary , p , fgF1 ) => let 

 val fgB = FG.build { pd=pd , forward=false , summary=summary , uDefInit=SOME false , uUseInit=SOME false , initialize=fn n => true , merge=fn ( a , b ) => ( a andalso b ) , equal=op = } 

 val nodes = MRS.nodes summary 

 val help = fn ( i , n ) => flattenOk ( PD.getConfig pd , fgF1 , fgB , n ) 

 val () = ID.foreach ( nodes , help ) 

 val state = S { summary=summary , flowgraph=fgB } 

 val env = E { pd=pd } 

 val () = Analyze2.analyseProgram ( state , env , p ) 

 val () = FG.propagate fgB  in fgB end 

 val forward2 = fn ( pd , summary , fgB ) => let 

 val fgF2 = FG.build { pd=pd , forward=true , summary=summary , uDefInit=SOME false , uUseInit=SOME false , initialize=fn n => FG.query ( fgB , n ) , merge=fn ( a , b ) => ( a andalso b ) , equal=op = } 

 val () = FG.propagate fgF2  in fgF2 end 

 val show = fn ( pd , summary , fg , p ) => if showFlattening pd then let 

 val si = Identifier.SymbolInfo.SiTable ( MU.Program.symbolTable p ) 

 val vars = MRS.listVariables summary 

 val unboxes = List.map ( vars , fn v => ( v , FG.query ( fg , MRS.variableNode ( summary , v ) ) ) ) 

 val lv = fn v => MilLayout.layoutVariable ( PD.getConfig pd , si , v ) 

 val ls = List.keepAllMap ( unboxes , fn ( v , unbox ) => if unbox then SOME ( lv v ) else NONE ) 

 val l = Layout.align ls 

 val l = Layout.align [ Layout.str "Flattening:" , LayoutUtils.indent l ] 

 val () = LayoutUtils.printLayout l  in ( ) end else ( ) structure Rewrite = struct 

 datatype rState = RS of { stm : M.symbolTableManager , extraGlobals : Mil.globals ref } 

 datatype rEnv = RE of { pd : PD.t , splits : Mil.variable vector VD.t } 

 val getPd = fn ( RE { pd , ... } ) => pd 

 val getConfig = PD.getConfig o getPd 

 val getSplits = fn ( RE { splits , ... } ) => splits 

 val getStm = fn ( RS { stm , extraGlobals } ) => stm 

 val getExtraGlobals = fn ( RS { stm , extraGlobals } ) => extraGlobals 

 val getSi = I.SymbolInfo.SiManager o getStm structure MS = MilStream 

 val splitVariable = fn ( state , env , v ) => VD.lookup ( getSplits env , v ) 

 val splitOperand = fn ( state , env , oper ) => ( case oper of M.SVariable v => Option.map ( splitVariable ( state , env , v ) , fn v => Vector.map ( v , M.SVariable ) )| _ => NONE ) 

 val splitVector = fn splitOne => fn ( state , env , items ) => let 

 val help = fn item => case splitOne ( state , env , item ) of SOME vs => vs| NONE => Vector.new1 ( item ) 

 val itemss = Vector.map ( items , help ) 

 val items = Vector.concatV itemss  in items end 

 val splitVariables = splitVector splitVariable 

 val splitOperands = splitVector splitOperand 

 val label = fn ( state , env , l , vs ) => let 

 val vs = splitVariables ( state , env , vs ) 

 val so = SOME ( l , vs , MS.empty )  in ( env , so ) end 

 val flattenOne = fn ( state , env , v , init ) => MS.bindRhs ( v , M.RhsSimple init ) 

 val flattenTuple = fn ( state , env , dests , { mdDesc , inits } ) => Try.try ( fn () => let 

 val v = Try.V.singleton dests 

 val vs = <@ splitVariable ( state , env , v ) 

 val pairs = Vector.zip ( vs , inits ) 

 val ss = Vector.toListMap ( pairs , fn ( v , init ) => flattenOne ( state , env , v , init ) ) 

 val s = MS.seqn ss 

 val () = Click.flatten ( getPd env )  in s end ) 

 val flattenMove = fn ( state , env , dests , v ) => Try.try ( fn () => let 

 val dv = Try.V.singleton dests 

 val dvs = <@ splitVariable ( state , env , dv ) 

 val vs = <@ splitVariable ( state , env , v ) 

 val fold = fn ( dv , v , s ) => let 

 val s0 = MS.bindRhs ( dv , M.RhsSimple ( M.SVariable v ) ) 

 val s1 = MS.seq ( s , s0 )  in s1 end 

 val s = Vector.fold2 ( dvs , vs , MS.empty , fold )  in s end ) 

 val flattenCondMov = fn ( state , env , dests , args ) => Try.try ( fn () => let 

 val dv = Try.V.singleton dests 

 val ( on , u , v ) = Try.V.tripleton args 

 val dvs = <@ splitVariable ( state , env , dv ) 

 val us = <@ splitOperand ( state , env , u ) 

 val vs = <@ splitOperand ( state , env , v ) 

 val n = Vector.size dvs 

 val () = Try.require ( n = Vector.size us ) 

 val () = Try.require ( n = Vector.size vs ) 

 val fold = fn ( dv , u , v , s ) => let 

 val s0 = MS.bindRhs ( dv , M.RhsPrim { prim=P.Prim P.PCondMov , createThunks=false , typs=Vector.new0 ( ) , args=Vector.new3 ( on , u , v ) } ) 

 val s1 = MS.seq ( s , s0 )  in s1 end 

 val s = Vector.fold3 ( dvs , us , vs , MS.empty , fold )  in s end ) 

 val instr = fn ( state , env , i as M.I { dests , n , rhs } ) => let 

 val so = ( case rhs of M.RhsSimple ( M.SVariable v ) => flattenMove ( state , env , dests , v )| M.RhsPrim { prim = P.Prim P.PCondMov , args = args , ... } => flattenCondMov ( state , env , dests , args )| M.RhsTuple r => flattenTuple ( state , env , dests , r )| M.RhsTupleSub tf => let 

 val v = MU.TupleField.tup tf 

 val res = Try.try ( fn () => let 

 val vs = <@ splitVariable ( state , env , v ) 

 val i = <@ MU.TupleField.fixed tf 

 val s = if i >= 0 andalso i < Vector.length vs then let 

 val vField = Vector.sub ( vs , i ) 

 val rhs = M.RhsSimple ( M.SVariable vField ) 

 val s = MS.bindsRhs ( dests , rhs )  in s end else let 

 val v = Vector.sub ( dests , 0 ) 

 val t = MT.Typer.variable ( getConfig env , getSi state , v ) 

 val vi = M.VI { typ=t , kind=M.VkGlobal } 

 val () = STM.variableSetInfo ( getStm state , v , vi ) 

 val g = M.GErrorVal t 

 val egs = getExtraGlobals state 

 val () = egs := VD.insert ( ! egs , v , g )  in MS.empty end  in s end )  in res end| _ => NONE )  in ( env , so ) end 

 val doTarget = fn ( state , env , M.T { block , arguments } ) => let 

 val arguments = splitOperands ( state , env , arguments )  in M.T { block=block , arguments=arguments } end 

 val doSwitch = fn ( state , env , { select , on , cases , default } ) => let 

 val help1 = fn tg => doTarget ( state , env , tg ) 

 val help2 = fn ( c , tg ) => ( c , help1 tg )  in { select=select , on=on , cases=Vector.map ( cases , help2 ) , default=Option.map ( default , help1 ) } end 

 val doInterProc = fn ( state , env , ip ) => ( case ip of M.IpEval _ => NONE| M.IpCall { call , args } => let 

 val args = splitOperands ( state , env , args )  in SOME ( M.IpCall { call=call , args=args } ) end ) 

 val transfer = fn ( state , env , t ) => let 

 val so = ( case t of M.TGoto tg => SOME ( MS.empty , M.TGoto ( doTarget ( state , env , tg ) ) )| M.TCase sw => SOME ( MS.empty , M.TCase ( doSwitch ( state , env , sw ) ) )| M.TInterProc r => Try.try ( fn () => let 

 val { callee , ret , fx } = r 

 val callee = <@ doInterProc ( state , env , callee ) 

 val r = { callee=callee , ret=ret , fx=fx } 

 val t = M.TInterProc r  in ( MS.empty , t ) end )| M.TReturn _ => NONE| M.TCut _ => NONE| M.THalt _ => NONE )  in ( env , so ) end 

 val global = fn ( state , env , v , g ) => let 

 val gso = ( case g of M.GCode f => Try.try ( fn () => let 

 val args = MU.Code.args f 

 val args = splitVariables ( state , env , args ) 

 val f = MU.Code.setArgs ( f , args ) 

 val g = M.GCode f 

 val t = MT.Typer.global ( getConfig env , getSi state , g ) 

 val () = STM.variableSetInfo ( getStm state , v , M.VI { typ=t , kind=M.VkGlobal } )  in [ ( v , g ) ] end )| M.GSimple ( M.SVariable x ) => Try.try ( fn () => let 

 val vs = <@ splitVariable ( state , env , v ) 

 val xs = <@ splitVariable ( state , env , x ) 

 val pairs = Vector.zip ( vs , xs ) 

 val gs = Vector.toListMap ( pairs , fn ( v , x ) => ( v , M.GSimple ( M.SVariable x ) ) )  in gs end )| M.GTuple { mdDesc , inits } => Try.try ( fn () => let 

 val vs = <@ splitVariable ( state , env , v ) 

 val pairs = Vector.zip ( vs , inits ) 

 val gs = Vector.toListMap ( pairs , fn ( v , s ) => ( v , M.GSimple s ) ) 

 val () = Click.flatten ( getPd env )  in gs end )| _ => NONE )  in ( env , gso ) end structure Transform = MilTransformF ( struct 

 type state = rState 

 type env = rEnv 

 val config = getConfig 

 val indent = 2 

 val label = label 

 val instr = instr 

 val transfer = transfer 

 val global = global  end ) 

 val globals = fn ( pd , stm , splits , globals ) => let 

 val extraGlobals = ref VD.empty 

 val state = RS { stm=stm , extraGlobals=extraGlobals } 

 val env = RE { pd=pd , splits=splits } 

 val globals = Transform.globals ( state , env , Transform.OAny , globals ) 

 val globals = VD.union ( globals , ! extraGlobals , fn ( k , a , b ) => fail ( "globals" , "Global name conflict" ) )  in globals end  end 

 val chooseSplitVars = fn ( pd , summary , stm , fgF1 , fgF2 ) => let 

 val doVariable = fn ( v , splits ) => let 

 val n = MRS.variableNode ( summary , v ) 

 val splits = if FG.query ( fgF2 , n ) then case Lat.get ( FG.query ( fgF1 , n ) ) of SOME es => let 

 val doOne = fn e => case TLat.get e of SOME ( _ , t ) => t| NONE => fail ( "chooseSplitVars" , "element is unknown" ) 

 val ts = Vector.map ( es , doOne ) 

 val kind = MSTM.variableKind ( stm , v ) 

 val newVar = fn ( i , t ) => let 

 val s = "_" ^ ( Int.toString i ) 

 val vi = MSTM.variableRelated ( stm , v , s , t , kind )  in vi end 

 val vs = Vector.mapi ( ts , newVar ) 

 val splits = VD.insert ( splits , v , vs )  in splits end| NONE => fail ( "chooseSplitVars" , "split var is not tuple" ) else splits  in splits end 

 val variables = MRS.listVariables summary 

 val splits = List.fold ( variables , VD.empty , doVariable )  in splits end 

 val rewrite = fn ( pd , summary , p , fgF1 , fgF2 ) => let 

 val M.P { includes , externs , globals , entry , symbolTable = _ } = p 

 val stm = I.Manager.fromExistingAll ( MU.Program.symbolTable p ) 

 val splits = chooseSplitVars ( pd , summary , stm , fgF1 , fgF2 ) 

 val globals = Rewrite.globals ( pd , stm , splits , globals ) 

 val st = I.Manager.finish stm 

 val p = M.P { includes=includes , externs=externs , globals=globals , entry=entry , symbolTable=st }  in p end 

 val program = fn ( pd , summary , p ) => let 

 val fgF1 = forward1 ( pd , summary , p ) 

 val fgB = backward ( pd , summary , p , fgF1 ) 

 val fgF2 = forward2 ( pd , summary , fgB ) 

 val () = show ( pd , summary , fgF2 , p ) 

 val p = rewrite ( pd , summary , p , fgF1 , fgF2 )  in p end  end 

 val program = Flatten.program  end 

