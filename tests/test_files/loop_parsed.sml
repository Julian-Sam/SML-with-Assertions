signature MIL_LOOP = sig type blocks = Mil.block Identifier.LabelDict.t datatype loop = L of { header : Mil.label , blocks : blocks } type loopTree = loop Tree.t type loopForest = loopTree Vector.t type t val build : Config.t * Mil.symbolInfo * MilCfg.t * ( Mil.label * Mil.block ) Tree.t -> t val fromLoops : Config.t * Mil.symbolInfo * { entry : Mil.label , loops : loopForest , blocksNotInLoops : blocks } -> t val unbuild : t -> Mil.codeBody val getEntry : t -> Mil.label val getLoops : t -> loopForest val getBlocksNotInLoops : t -> blocks val genAllNodes : t -> t val getAllNodes : t * Mil.label -> blocks val allNodes : t -> blocks Identifier.LabelDict.t val genExits : t -> t val getExits : t * Mil.label -> Identifier.LabelSet.t val allExits : t -> Identifier.LabelSet.t Identifier.LabelDict.t val addPreheaders : t * Mil.symbolTableManager -> t val getPreheader : t * Mil.label -> Mil.label option val getPreheaders : t -> Mil.label Identifier.LabelDict.t datatype inductionVariable = BIV of { variable : Mil.variable , init : Mil.operand , step : Rat.t } | DIV of { variable : Mil.variable , base : { variable : Mil.variable , init : Mil.operand , step : Rat.t } , scale : Rat.t , offset : Rat.t } val canonizeInductionVariable : inductionVariable -> { variable : Mil.variable , init : Rat.t * Mil.operand * Rat.t , step : Rat.t } val genInductionVariables : t * FMil.t * MilCfg.t -> t val getInductionVariables : t * Mil.label -> inductionVariable list val inductionVars : t -> inductionVariable list Identifier.LabelDict.t datatype tripCount = TC of { block : Mil.label , cond : Mil.variable , flip1 : bool , comparison : Mil.Prims.compareOp , flip2 : bool , init : Rat.t * Mil.operand * Rat.t , step : Rat.t , bound : Mil.operand } val genTripCounts : t * FMil.t * MilCfg.t * ( Mil.label * Mil.block ) Tree.t -> t val getTripCount : t * Mil.label -> tripCount option val allTripCounts : t -> tripCount Identifier.LabelDict.t val genBinderLocations : t -> t val getBinderLocations : t -> Mil.label Identifier.VariableDict.t val layout : Config.t * Mil.symbolInfo * t -> Layout.t val layoutInductionVariable : Config.t * Mil.symbolInfo * inductionVariable -> Layout.t  end ; structure MilLoop :> MIL_LOOP = struct 

 val moduleName = "MilLoop" 

 fun fail ( f , m ) = Fail.fail ( moduleName , f , m )  structure L = Layout structure LU = LayoutUtils structure I = Identifier structure VD = I.VariableDict structure VS = I.VariableSet structure LD = I.LabelDict structure LS = I.LabelSet structure P = Mil.Prims structure M = Mil structure MU = MilUtils structure PU = MilUtils.Prims.Utils structure MSTM = MU.SymbolTableManager structure Cfg = MilCfg structure L = Layout structure LU = LayoutUtils structure ML = MilLayout 

 type blocks = M.block LD.t 

 datatype loop = L of { header : M.label , blocks : blocks } 

 type loopTree = loop Tree.t 

 type loopForest = loopTree Vector.t 

 datatype inductionVariable = BIV of { variable : Mil.variable , init : Mil.operand , step : Rat.t } | DIV of { variable : Mil.variable , base : { variable : Mil.variable , init : Mil.operand , step : Rat.t } , scale : Rat.t , offset : Rat.t } 

 datatype tripCount = TC of { block : M.label , cond : M.variable , flip1 : bool , comparison : P.compareOp , flip2 : bool , init : Rat.t * M.operand * Rat.t , step : Rat.t , bound : M.operand } 

 datatype t = LS of { config : Config.t , si : M.symbolInfo , entry : M.label , loops : loopForest , blocksNotInLoops : blocks , allNodes : blocks LD.t option , exits : LS.t LD.t option , preheaders : M.label LD.t option , inductionVars : inductionVariable list LD.t option , tripCounts : tripCount LD.t option , binderLocations : Mil.label Identifier.VariableDict.t } 

 datatype buildState = S of { inLoops : LS.t LD.t ref , top : blocks ref , loops : blocks LD.t ref , topLoops : loopTree list ref , subloops : loopTree list LD.t ref } 

 fun buildStateMk () = S { inLoops=ref LD.empty , top=ref LD.empty , loops=ref LD.empty , topLoops=ref [ ] , subloops=ref LD.empty }  

 fun getInLoops ( S { inLoops , ... } , l ) = Utils.Option.get ( LD.lookup ( ! inLoops , l ) , LS.empty )  

 fun addInLoop ( s as S { inLoops , ... } , l , h ) = inLoops := LD.insert ( ! inLoops , l , LS.insert ( getInLoops ( s , l ) , h ) )  

 fun getTop ( S { top , ... } ) = ! top  

 fun addTop ( S { top , ... } , l , b ) = top := LD.insert ( ! top , l , b )  

 fun getLoop ( S { loops , ... } , h ) = LD.lookup ( ! loops , h )  

 fun getLoop' ( s , h ) = Utils.Option.get ( getLoop ( s , h ) , LD.empty )  

 fun addLoop ( s as S { loops , ... } , l , b , h ) = loops := LD.insert ( ! loops , h , LD.insert ( getLoop' ( s , h ) , l , b ) )  

 fun getTopLoops ( S { topLoops , ... } ) = Vector.fromList ( List.rev ( ! topLoops ) )  

 fun addTopLoop ( S { topLoops , ... } , l ) = topLoops := l :: ( ! topLoops )  

 fun getSubloops ( S { subloops , ... } , h ) = let 

 val ls = Utils.Option.get ( LD.lookup ( ! subloops , h ) , [ ] ) 

 val ls = Vector.fromList ( List.rev ls )  in ls end  

 fun addSubloop ( S { subloops , ... } , h , l ) = let 

 val ls = Utils.Option.get ( LD.lookup ( ! subloops , h ) , [ ] ) 

 val ls = l :: ls 

 val () = subloops := LD.insert ( ! subloops , h , ls )  in ( ) end  

 datatype buildEnv = E of { config : Config.t , si : M.symbolInfo } 

 fun buildEnvMk ( c , si ) = E { config=c , si=si }  

 fun addNaturalLoop ( state , env , cfg , l , h ) = let 

 fun loop ( visited , stk ) = case stk of [ ] => ( )| ( l , n ) :: stk => let 

 val () = addInLoop ( state , l , h ) 

 val visited = LS.insert ( visited , l ) 

 fun doOne ( n , stk ) = case Cfg.nodeGetLabel ( cfg , n ) of NONE => stk| SOME l => if LS.member ( visited , l ) then stk else ( l , n ) :: stk  

 val stk = if l <> h then List.fold ( Cfg.pred ( cfg , n ) , stk , doOne ) else stk  in loop ( visited , stk ) end  

 val n = Cfg.labelGetNode ( cfg , l ) 

 val () = loop ( LS.empty , [ ( l , n ) ] )  in ( ) end  

 fun determineInLoops ( state , env , cfg , dt , ds ) = let 

 val Tree.T ( ( l , b ) , children ) = dt 

 val ds = LS.insert ( ds , l ) 

 fun doTarget t = if LS.member ( ds , t ) then addNaturalLoop ( state , env , cfg , l , t ) else ( )  

 val () = LS.foreach ( # blocks ( MU.Block.successors b ) , doTarget ) 

 fun doChild c = determineInLoops ( state , env , cfg , c , ds )  

 val () = Vector.foreach ( children , doChild )  in ( ) end  

 fun buildLoops ( cfg , state , env , dt , ds ) = let 

 val Tree.T ( ( l , b ) , children ) = dt 

 val ds' = l :: ds 

 fun doChild c = buildLoops ( cfg , state , env , c , ds' )  

 val () = Vector.foreach ( children , doChild ) 

 val myLoops = getInLoops ( state , l ) 

 fun detLoop ds = List.peek ( ds , fn h => LS.member ( myLoops , h ) )  

 val () = case detLoop ds' of NONE => addTop ( state , l , b )| SOME h => addLoop ( state , l , b , h ) 

 val () = case getLoop ( state , l ) of NONE => ( )| SOME blks => let 

 val loop = L { header=l , blocks=blks } 

 val sl = getSubloops ( state , l ) 

 val lt = Tree.T ( loop , sl )  in case detLoop ds of NONE => addTopLoop ( state , lt )| SOME h => addSubloop ( state , h , lt ) end  in ( ) end  

 fun fromLoops ( c , si , { entry , loops , blocksNotInLoops } ) = LS { config=c , si=si , entry=entry , loops=loops , blocksNotInLoops=blocksNotInLoops , allNodes=NONE , exits=NONE , preheaders=NONE , inductionVars=NONE , tripCounts=NONE , binderLocations=VD.empty }  

 fun build ( c , si , cfg , dt ) = let 

 val entry = Cfg.startLabel cfg 

 val state = buildStateMk ( ) 

 val env = buildEnvMk ( c , si ) 

 val () = determineInLoops ( state , env , cfg , dt , LS.empty ) 

 val () = buildLoops ( cfg , state , env , dt , [ ] ) 

 val loops = getTopLoops state 

 val top = getTop state 

 val r = fromLoops ( c , si , { entry=entry , loops=loops , blocksNotInLoops=top } )  in r end  

 fun getConfig ( LS { config , ... } ) = config  

 fun getSi ( LS { si , ... } ) = si  

 fun getEntry ( LS { entry , ... } ) = entry  

 fun getLoops ( LS { loops , ... } ) = loops  

 fun getBlocksNotInLoops ( LS { blocksNotInLoops , ... } ) = blocksNotInLoops  

 fun unbuild ls = let 

 fun doLoop ( L { header , blocks } , nblks ) = LD.insertAll ( nblks , LD.toList blocks )  

 fun doLoopTree ( lt , nblks ) = Tree.foldPre ( lt , nblks , doLoop )  

 val allBlks = Vector.fold ( getLoops ls , getBlocksNotInLoops ls , doLoopTree )  in M.CB { entry=getEntry ls , blocks=allBlks } end  

 fun genAllNodes ls = let 

 val LS { config , si , entry , loops , blocksNotInLoops , allNodes , exits , preheaders , inductionVars , tripCounts , binderLocations } = ls 

 val allNodes = ref LD.empty 

 fun addNodes ( h , ns ) = allNodes := LD.insert ( ! allNodes , h , ns )  

 fun doLoop l = let 

 val Tree.T ( L { header , blocks , ... } , children ) = l 

 fun doChild ( c , nodes ) = let 

 val childNodes = doLoop c 

 fun doOne ( l , b , nodes ) = LD.insert ( nodes , l , b )  

 val nodes = LD.fold ( childNodes , nodes , doOne )  in nodes end  

 val nodes = Vector.fold ( children , blocks , doChild ) 

 val () = addNodes ( header , nodes )  in nodes end  

 fun doOne l = let 

 val _ = doLoop l  in ( ) end  

 val () = Vector.foreach ( loops , doOne ) 

 val r = LS { config=config , si=si , entry=entry , loops=loops , blocksNotInLoops=blocksNotInLoops , allNodes=SOME ( ! allNodes ) , exits=exits , preheaders=preheaders , inductionVars=inductionVars , tripCounts=tripCounts , binderLocations=binderLocations }  in r end  

 fun allNodes ( LS { allNodes , ... } ) = ( case allNodes of SOME an => an| NONE => fail ( "allNodes" , "all nodes have not been generated" ) )  

 fun getAllNodes ( ls , h ) = ( case LD.lookup ( allNodes ls , h ) of NONE => fail ( "getAllNodes" , "no all nodes for header: " ^ I.labelString h )| SOME ns => ns )  

 fun genExits ls = let 

 val LS { config , si , entry , loops , blocksNotInLoops , allNodes , exits , preheaders , inductionVars , tripCounts , binderLocations } = ls 

 val exits = ref LD.empty 

 fun addExits ( h , es ) = exits := LD.insert ( ! exits , h , es )  

 fun doLoop l = let 

 val Tree.T ( L { header , ... } , children ) = l 

 val myNodes = getAllNodes ( ls , header ) 

 fun doOne ( l , b , es ) = let 

 val { blocks , exits } = MU.Block.successors b 

 fun checkOne l = not ( LD.contains ( myNodes , l ) )  

 val es = if exits orelse LS.exists ( blocks , checkOne ) then LS.insert ( es , l ) else es  in es end  

 val es = LD.fold ( myNodes , LS.empty , doOne ) 

 val () = addExits ( header , es ) 

 val () = Vector.foreach ( children , doLoop )  in ( ) end  

 val () = Vector.foreach ( loops , doLoop ) 

 val r = LS { config=config , si=si , entry=entry , loops=loops , blocksNotInLoops=blocksNotInLoops , allNodes=allNodes , exits=SOME ( ! exits ) , preheaders=preheaders , inductionVars=inductionVars , tripCounts=tripCounts , binderLocations=binderLocations }  in r end  

 fun allExits ( LS { exits , ... } ) = ( case exits of SOME e => e| NONE => fail ( "allExits" , "exits have not been generated" ) )  

 fun getExits ( ls , h ) = case LD.lookup ( allExits ls , h ) of NONE => fail ( "getExits" , "no exits for header: " ^ I.labelString h )| SOME ns => ns  

 fun cutsInBlock ( ls , b , cs ) = LS.union ( cs , MU.Cuts.targets ( MU.Block.cuts b ) )  

 fun cutsInLoop ( ls , l , cs ) = let 

 val L { blocks , ... } = l 

 fun doOne ( _ , b , cs ) = cutsInBlock ( ls , b , cs )  

 val cs = LD.fold ( blocks , cs , doOne )  in cs end  

 fun cutsInLoops ( ls , cs ) = let 

 val Tree.T ( l , children ) = ls 

 val cs = cutsInLoop ( ls , l , cs ) 

 fun doOne ( ls , cs ) = cutsInLoops ( ls , cs )  

 val cs = Vector.fold ( children , cs , doOne )  in cs end  

 fun cuts ls = let 

 fun doOne ( _ , b , cs ) = cutsInBlock ( ls , b , cs )  

 val cs = LD.fold ( getBlocksNotInLoops ls , LS.empty , doOne ) 

 fun doOne ( ls , cs ) = cutsInLoops ( ls , cs )  

 val cs = Vector.fold ( getLoops ls , cs , doOne )  in cs end  

 fun genPreheaders ( ls , stm , cuts ) = let 

 fun doOne ( Tree.T ( L { header , blocks , ... } , children ) , phs ) = let 

 val phs = if LS.member ( cuts , header ) then phs else let 

 val ph = MSTM.labelFresh stm  in LD.insert ( phs , header , ph ) end 

 val phs = Vector.fold ( children , phs , doOne )  in phs end  

 val phs = Vector.fold ( getLoops ls , LD.empty , doOne )  in phs end  

 fun retargetBlock ( ls , phs , b ) = let 

 val M.B { parameters = ps , instructions = is , transfer = t } = b 

 val t = MilRename.Label.transfer ( getConfig ls , phs , t ) 

 val b = M.B { parameters=ps , instructions=is , transfer=t }  in b end  

 fun genPreheaderBlock ( ls , stm , ph , h , blocks ) = let 

 val hb = Option.valOf ( LD.lookup ( blocks , h ) ) 

 val M.B { parameters = ps , ... } = hb 

 val nps = Vector.map ( ps , fn p => MSTM.variableClone ( stm , p ) ) 

 val gas = Vector.map ( nps , M.SVariable ) 

 val t = M.TGoto ( M.T { block=h , arguments=gas } ) 

 val phb = M.B { parameters=nps , instructions=Vector.new0 ( ) , transfer=t }  in phb end  

 fun addPreheadersA ( ls , stm , phs ) = let 

 val LS { config , si , entry , loops , blocksNotInLoops , allNodes , exits , preheaders , inductionVars , tripCounts , binderLocations } = ls 

 val entry = Utils.Option.get ( LD.lookup ( phs , entry ) , entry ) 

 val blks = LD.map ( blocksNotInLoops , fn ( _ , b ) => retargetBlock ( ls , phs , b ) ) 

 fun doLoop phs ( Tree.T ( L { header , blocks } , children ) , blks ) = let 

 val blks = case LD.lookup ( phs , header ) of NONE => blks| SOME ph => let 

 val phb = genPreheaderBlock ( ls , stm , ph , header , blocks )  in LD.insert ( blks , ph , phb ) end 

 val phs = LD.remove ( phs , header ) 

 fun doBlk ( _ , b ) = retargetBlock ( ls , phs , b )  

 val blocks = LD.map ( blocks , doBlk ) 

 val ( children , blocks ) = Vector.mapAndFold ( children , blocks , doLoop phs ) 

 val ls = Tree.T ( L { header=header , blocks=blocks } , children )  in ( ls , blks ) end  

 val ( loops , blks ) = Vector.mapAndFold ( loops , blks , doLoop phs ) 

 val r = LS { config=config , si=si , entry=entry , loops=loops , blocksNotInLoops=blks , allNodes=NONE , exits=exits , preheaders=SOME phs , inductionVars=inductionVars , tripCounts=tripCounts , binderLocations=binderLocations }  in r end  

 fun addPreheaders ( ls , stm ) = let 

 val cuts = cuts ls 

 val phs = genPreheaders ( ls , stm , cuts ) 

 val ls = addPreheadersA ( ls , stm , phs )  in ls end  

 fun getPreheaders ( LS { preheaders , ... } ) = ( case preheaders of SOME ph => ph| NONE => fail ( "getPreheaders" , "preheaders have not been generated" ) )  

 fun getPreheader ( ls , h ) = LD.lookup ( getPreheaders ls , h )  

 fun canonizeInductionVariable ( iv : inductionVariable ) : { variable : Mil.variable , init : Rat.t * Mil.operand * Rat.t , step : Rat.t } = ( case iv of BIV { variable , init , step } => { variable=variable , init=( Rat.one , init , Rat.zero ) , step=step }| DIV { variable = vd , base = { variable = vb , init , step } , scale , offset } => { variable=vd , init=( scale , init , offset ) , step=Rat.* ( scale , step ) } )  

 type linearFunction = { var : M.variable , m : Rat.t , c : Rat.t } 

 fun layoutLinearFunction ( { var , m , c } ) = L.seq [ Rat.layout m , L.str "*" , I.layoutVariable' var , L.str "+" , Rat.layout c ]  

 datatype value = VConstant of Rat.t | VLf of linearFunction 

 fun layoutValue v = case v of VConstant r => Rat.layout r| VLf lf => layoutLinearFunction lf  

 datatype ivState = S of { vals : value VD.t ref , ivs : inductionVariable List.t LD.t ref , bivs : { hdr : M.label , init : M.operand , step : Rat.t } VD.t ref } 

 fun ivStateMk () = S { vals=ref VD.empty , ivs=ref LD.empty , bivs=ref VD.empty }  

 fun getValue ( S { vals , ... } , v ) = VD.lookup ( ! vals , v )  

 fun getValues ( S { vals , ... } ) = ! vals  

 fun addValue ( S { vals , ... } , vr , vl ) = vals := VD.insert ( ! vals , vr , vl )  

 fun addInductionVariable ( S { ivs , ... } , h , iv ) = let 

 val ivl = Utils.Option.get ( LD.lookup ( ! ivs , h ) , [ ] ) 

 val ivl = iv :: ivl 

 val () = ivs := LD.insert ( ! ivs , h , ivl )  in ( ) end  

 fun getBasicInductionVariable ( S { bivs , ... } , v ) = VD.lookup ( ! bivs , v )  

 fun addBasicInductionVariable ( s as S { bivs , ... } , h , v , init , step ) = let 

 val iv = BIV { variable=v , init=init , step=step } 

 val () = addInductionVariable ( s , h , iv ) 

 val () = bivs := VD.insert ( ! bivs , v , { hdr=h , init=init , step=step } )  in ( ) end  

 fun ivStateFinish ( S { ivs , ... } ) = ! ivs  

 fun layoutValues ( S { vals , ... } ) = let 

 fun doOne ( v , vl ) = L.seq [ I.layoutVariable' v , L.str ": " , layoutValue vl ]  

 val l = L.align ( List.map ( VD.toList ( ! vals ) , doOne ) ) 

 val l = L.align [ L.str "Values:" , LU.indent l ]  in l end  local  open Rat  in 

 fun genValues ( state , env , fmil , ls ) = let 

 val visited = ref VS.empty 

 fun isVisited v = VS.member ( ! visited , v )  

 fun addVisited v = visited := VS.insert ( ! visited , v )  

 fun getVarValue v = if isVisited v then getValue ( state , v ) else let 

 val () = addVisited v 

 val v' = getVarValueA v 

 val () = case v' of NONE => ( )| SOME v' => addValue ( state , v , v' )  in v' end and getVarValueA v = case FMil.getVariable ( fmil , v ) of FMil.VdExtern _ => NONE| FMil.VdGlobal g => getGlobalValue g| FMil.VdFunParam _ => NONE| FMil.VdLabParam _ => NONE| FMil.VdInstr ( _ , rhs ) => getRhsValue rhs| FMil.VdRetVar _ => NONE and getGlobalValue g = case g of M.GRat r => SOME ( VConstant r )| M.GInteger i => SOME ( VConstant ( Rat.fromIntInf i ) )| M.GSimple s => getOperandValue s| _ => NONE and getRhsValue rhs = case rhs of M.RhsSimple s => getOperandValue s| M.RhsPrim x => getPrimValue x| _ => NONE and getPrimValue { prim , args , ... } = let 

 fun unary f = case getOperandValue ( Vector.sub ( args , 0 ) ) of NONE => NONE| SOME v => f v  

 fun binary f = case ( getOperandValue ( Vector.sub ( args , 0 ) ) , getOperandValue ( Vector.sub ( args , 1 ) ) ) of ( SOME v1 , SOME v2 ) => f ( v1 , v2 )| _ => NONE   in case prim of P.Prim ( P.PNumArith r ) => ( case # operator r of P.APlus => binary getPlusValue| P.ANegate => unary getNegateValue| P.AMinus => binary getMinusValue| P.ATimes => binary getTimesValue| _ => NONE )| P.Prim ( P.PNumConvert _ ) => unary ( fn v => SOME v )| _ => NONE end and binaryLf ( x1 , x2 , f ) = if # var x1 = # var x2 then let 

 val m = f ( # m x1 , # m x2 ) 

 val c = f ( # c x1 , # c x2 )  in if equals ( m , zero ) then SOME ( VConstant c ) else SOME ( VLf { var=# var x1 , m=m , c=c } ) end else NONE and getPlusValue ( v1 , v2 ) = case ( v1 , v2 ) of ( VConstant r1 , VConstant r2 ) => SOME ( VConstant ( r1 + r2 ) )| ( VConstant r1 , VLf { var , m , c } ) => SOME ( VLf { var=var , m=m , c=r1 + c } )| ( VLf { var , m , c } , VConstant r2 ) => SOME ( VLf { var=var , m=m , c=c + r2 } )| ( VLf x1 , VLf x2 ) => binaryLf ( x1 , x2 , Rat.+ ) and getNegateValue v = case v of VConstant r => SOME ( VConstant ( ~ r ) )| VLf { var , m , c } => SOME ( VLf { var=var , m=~ m , c=~ c } ) and getMinusValue ( v1 , v2 ) = case ( v1 , v2 ) of ( VConstant r1 , VConstant r2 ) => SOME ( VConstant ( r1 - r2 ) )| ( VConstant r1 , VLf { var , m , c } ) => SOME ( VLf { var=var , m=~ m , c=r1 - c } )| ( VLf { var , m , c } , VConstant r2 ) => SOME ( VLf { var=var , m=m , c=c - r2 } )| ( VLf x1 , VLf x2 ) => binaryLf ( x1 , x2 , Rat.- ) and getTimesValue ( v1 , v2 ) = case ( v1 , v2 ) of ( VConstant r1 , VConstant r2 ) => SOME ( VConstant ( r1 * r2 ) )| ( VConstant r1 , VLf { var , m , c } ) => SOME ( VLf { var=var , m=r1 * m , c=r1 * c } )| ( VLf { var , m , c } , VConstant r2 ) => SOME ( VLf { var=var , m=m * r2 , c=c * r2 } )| ( VLf _ , VLf _ ) => NONE and getOperandValue opnd = case opnd of M.SConstant c => ( case c of M.CRat r => SOME ( VConstant ( Rat.fromIntInf r ) )| M.CInteger i => SOME ( VConstant ( Rat.fromIntInf i ) )| M.CIntegral i => SOME ( VConstant ( Rat.fromIntInf ( IntArb.toIntInf i ) ) )| _ => NONE )| M.SVariable v => ( case getVarValue v of NONE => SOME ( VLf { var=v , m=Rat.one , c=Rat.zero } )| SOME v' => SOME v' )  

 fun doInstruction i = Vector.foreach ( MU.Instruction.dests i , ignore o getVarValue )  

 fun doBlock ( _ , b ) = Vector.foreach ( MU.Block.instructions b , doInstruction )  

 fun doLoop ( L { blocks , ... } ) = LD.foreach ( blocks , doBlock )  

 fun doLoopTree ( Tree.T ( l , ls ) ) = let 

 val () = doLoop l 

 val () = Vector.foreach ( ls , doLoopTree )  in ( ) end  

 val () = LD.foreach ( getBlocksNotInLoops ls , doBlock ) 

 val () = Vector.foreach ( getLoops ls , doLoopTree )  in ( ) end   end 

 datatype initAnalysis = IaUndetermined | IaOperand of M.operand | IaUnknown 

 datatype loopAnalysis = LaUndetermined | LaIv of Rat.t | LaUnknown 

 fun layoutInitAnalysis ( env , ia ) = case ia of IaUndetermined => Layout.str "undet"| IaOperand opnd => MilLayout.layoutOperand ( getConfig env , getSi env , opnd )| IaUnknown => Layout.str "unknown"  

 fun layoutLoopAnalysis la = case la of LaUndetermined => Layout.str "undet"| LaIv r => Rat.layout r| LaUnknown => Layout.str "unknown"  

 fun genBasicInductionVariables ( state , env , cfg , ls ) = let 

 val LS { loops , ... } = ls 

 fun doLoopTree lt = let 

 val Tree.T ( L { header , blocks , ... } , children ) = lt 

 val hb = Option.valOf ( LD.lookup ( blocks , header ) ) 

 val params = MU.Block.parameters hb 

 val M.B { parameters , ... } = hb 

 val allNodes = getAllNodes ( ls , header ) 

 fun doOne p = ( p , IaUndetermined , LaUndetermined )  

 val a = Vector.map ( params , doOne ) 

 fun failed () = fail ( "genBasicInductionVariables.doEdges" , "bad transfer" )  

 fun doTarget ( a , t , f1 , f2 ) = Vector.map2 ( a , MU.Target.arguments t , f1 )  

 fun doSwitch ( a , s , f1 , f2 ) = let 

 val { cases , default , ... } = s 

 fun doOne ( ( _ , t ) , a ) = if MU.Target.block t = header then doTarget ( a , t , f1 , f2 ) else a  

 val a = Vector.fold ( cases , a , doOne ) 

 val a = case default of NONE => a| SOME t => if MU.Target.block t = header then doTarget ( a , t , f1 , f2 ) else a  in a end  

 fun doEdges ( a , b , f1 , f2 ) = case MU.Block.transfer b of M.TGoto t => doTarget ( a , t , f1 , f2 )| M.TCase s => doSwitch ( a , s , f1 , f2 )| M.TInterProc _ => Vector.map ( a , f2 )| M.TReturn _ => failed ( )| M.TCut _ => Vector.map ( a , f2 )| M.THalt _ => failed ( )  

 fun doInitUnknown ( p , _ , la ) = ( p , IaUnknown , la )  

 fun doInitArg ( ( p , ia , la ) , arg ) = case ia of IaUndetermined => ( p , IaOperand arg , la )| IaOperand opnd => if MU.Operand.compare ( opnd , arg ) = EQUAL then ( p , ia , la ) else ( p , IaUnknown , la )| IaUnknown => ( p , ia , la )  

 fun doLoopUnknown ( p , ia , _ ) = ( p , ia , LaUnknown )  

 fun getValueO opnd = case opnd of M.SVariable v => getValue ( state , v )| _ => NONE  

 fun doLoopArg ( ( p , ia , la ) , arg ) = case la of LaUndetermined => ( case getValueO arg of NONE => ( p , ia , LaUnknown )| SOME ( VConstant _ ) => ( p , ia , LaUnknown )| SOME ( VLf { var , m , c } ) => if Rat.equals ( m , Rat.one ) andalso var = p then ( p , ia , LaIv c ) else ( p , ia , LaUnknown ) )| LaIv step => ( case getValueO arg of NONE => ( p , ia , LaUnknown )| SOME ( VConstant _ ) => ( p , ia , LaUnknown )| SOME ( VLf { var , m , c } ) => if Rat.equals ( m , Rat.one ) andalso var = p andalso Rat.equals ( c , step ) then ( p , ia , la ) else ( p , ia , LaUnknown ) )| LaUnknown => ( p , ia , la )  

 fun unknown ( p , _ , _ ) = ( p , IaUnknown , LaUnknown )  

 fun doPred ( pred , a ) = case Cfg.nodeGetLabelBlock ( cfg , pred ) of NONE => Vector.map ( a , unknown )| SOME ( pl , pb ) => if LD.contains ( allNodes , pl ) then doEdges ( a , pb , doLoopArg , doLoopUnknown ) else doEdges ( a , pb , doInitArg , doInitUnknown )  

 val preds = Cfg.pred ( cfg , Cfg.labelGetNode ( cfg , header ) ) 

 val a = List.fold ( preds , a , doPred ) 

 fun doOne ( p , ia , la ) = case ( ia , la ) of ( IaOperand init , LaIv step ) => addBasicInductionVariable ( state , header , p , init , step )| _ => ( )  

 val () = Vector.foreach ( a , doOne ) 

 val () = Vector.foreach ( children , doLoopTree )  in ( ) end  

 val () = Vector.foreach ( loops , doLoopTree )  in ( ) end  

 fun genDerivedInductionVariables ( state , env ) = let 

 fun doOne ( v , vl ) = case vl of VLf { var , m , c } => ( case getBasicInductionVariable ( state , var ) of NONE => ( )| SOME { hdr , init , step } => let 

 val iv = DIV { variable=v , base={ variable=var , init=init , step=step } , scale=m , offset=c } 

 val () = addInductionVariable ( state , hdr , iv )  in ( ) end )| _ => ( )  

 val () = VD.foreach ( getValues state , doOne )  in ( ) end  

 fun genInductionVariables ( ls , fmil , cfg ) = let 

 val state = ivStateMk ( ) 

 val env = ls 

 val () = genValues ( state , env , fmil , ls ) 

 val () = genBasicInductionVariables ( state , env , cfg , ls ) 

 val () = genDerivedInductionVariables ( state , env ) 

 val ivs = ivStateFinish state 

 val LS x = ls 

 val r = LS { config=# config x , si=# si x , entry=# entry x , loops=# loops x , blocksNotInLoops=# blocksNotInLoops x , allNodes=# allNodes x , exits=# exits x , preheaders=# preheaders x , inductionVars=SOME ivs , tripCounts=# tripCounts x , binderLocations=# binderLocations x }  in r end  

 fun genInductionVariables' ( fmil , cfg ) ls = genInductionVariables ( ls , fmil , cfg )  

 fun inductionVars ( LS { inductionVars , ... } ) = ( case inductionVars of SOME ivs => ivs| NONE => fail ( "inductionVars" , "induction variables have not been generated" ) )  

 fun getInductionVariables ( ls , h ) = case LD.lookup ( inductionVars ls , h ) of NONE => [ ]| SOME ivs => ivs  

 datatype leState = S of { allDefs : VS.t LD.t ref , tcs : tripCount LD.t ref } 

 fun tcStateMk () = S { allDefs=ref LD.empty , tcs=ref LD.empty }  

 fun isDef ( S { allDefs , ... } , h , v ) = VS.member ( Option.valOf ( LD.lookup ( ! allDefs , h ) ) , v )  

 fun addDefs ( S { allDefs , ... } , h , vs ) = allDefs := LD.insert ( ! allDefs , h , vs )  

 fun addTripCount ( S { tcs , ... } , h , tc ) = tcs := LD.insert ( ! tcs , h , tc )  

 fun tcStateFinish ( S { tcs , ... } ) = ! tcs  

 fun genDefs ( state , env , ls ) = let 

 fun doInstruction ( M.I { dests , ... } , defs ) = Vector.fold ( dests , defs , VS.insert o Utils.flip2 )  

 fun doBlock ( l , b , defs ) = let 

 val M.B { parameters , instructions , ... } = b 

 val defs = Vector.fold ( parameters , defs , VS.insert o Utils.flip2 ) 

 val defs = Vector.fold ( instructions , defs , doInstruction )  in defs end  

 fun doLoop l = let 

 val Tree.T ( L { header , blocks , ... } , children ) = l 

 val myDefs = LD.fold ( blocks , VS.empty , doBlock ) 

 val childDefs = Vector.map ( children , doLoop ) 

 val defs = Vector.fold ( childDefs , myDefs , VS.union ) 

 val () = addDefs ( state , header , defs )  in defs end  

 fun doOne l = let 

 val _ = doLoop l  in ( ) end  

 val () = Vector.foreach ( getLoops ls , doOne )  in ( ) end  

 datatype leAnalysis = ALoopInvariant | AInductionVariable of ( Rat.t * M.operand * Rat.t ) * Rat.t | AUnknown 

 fun analyseOperand ( state , env , h , myIvs , opnd ) = case opnd of M.SVariable v => let 

 fun pred iv = ( case iv of BIV { variable , ... } => v = variable| DIV { variable , ... } => v = variable )  

 val al = ( case List.peek ( myIvs , pred ) of SOME iv => let 

 val { init , step , ... } = canonizeInductionVariable iv  in AInductionVariable ( init , step ) end| NONE => if isDef ( state , h , v ) then AUnknown else ALoopInvariant )  in al end| M.SConstant _ => ALoopInvariant  

 fun loopTripCount ( state , env , fmil , cfg , lDomInfo , h ) = Try.exec ( fn () => let 

 val myNodes = getAllNodes ( env , h ) 

 val myExits = getExits ( env , h ) 

 val myExit = case LS.toList myExits of [ exit ] => exit| _ => Try.fail ( ) 

 val myNode = Cfg.labelGetNode ( cfg , h ) 

 val myPreds = Cfg.pred ( cfg , myNode ) 

 fun prjOne n = Cfg.nodeGetLabel ( cfg , n )  

 val myPreds = List.keepAllMap ( myPreds , prjOne ) 

 fun checkOne l = not ( LD.contains ( myNodes , l ) ) orelse MilCfg.LabelDominance.dominates ( lDomInfo , myExit , l )  

 val () = Try.require ( List.forall ( myPreds , checkOne ) ) 

 val { on , trueBranch , falseBranch } = Try.<- ( MU.Transfer.isBoolIf ( MU.Block.transfer ( Option.valOf ( LD.lookup ( myNodes , myExit ) ) ) ) ) 

 val lt = MU.Target.block trueBranch 

 val lf = MU.Target.block falseBranch 

 val tne = LD.contains ( myNodes , lt ) 

 val fne = LD.contains ( myNodes , lf ) 

 val flip1 = if tne andalso not fne then true else if not tne andalso fne then false else fail ( "loopTripCount" , "bad exit" ) 

 val v = case on of M.SConstant _ => Try.fail ( )| M.SVariable v => v 

 val ( cmp , o1 , o2 ) = case FMil.getVariable ( fmil , v ) of FMil.VdInstr ( _ , M.RhsPrim { prim = P.Prim ( P.PNumCompare r ) , args , ... } ) => ( # operator r , Vector.sub ( args , 0 ) , Vector.sub ( args , 1 ) )| _ => Try.fail ( ) 

 val myIvs = getInductionVariables ( env , h ) 

 val ( ( init , step ) , bnd , flip2 ) = case ( analyseOperand ( state , env , h , myIvs , o1 ) , analyseOperand ( state , env , h , myIvs , o2 ) ) of ( ALoopInvariant , AInductionVariable iv ) => ( iv , o1 , true )| ( AInductionVariable iv , ALoopInvariant ) => ( iv , o2 , false )| _ => Try.fail ( ) 

 val tc = TC { block=myExit , cond=v , flip1=flip1 , comparison=cmp , flip2=flip2 , init=init , step=step , bound=bnd } 

 val () = addTripCount ( state , h , tc )  in ( ) end )  

 fun genTripCounts ( ls , fmil , cfg , dt ) = let 

 val labelDomTree = Tree.map ( dt , fn ( l , b ) => l ) 

 val labelDominanceInfo = MilCfg.LabelDominance.new labelDomTree 

 val state = tcStateMk ( ) 

 val env = ls 

 val () = genDefs ( state , env , ls ) 

 fun doLoop l = let 

 val Tree.T ( L { header , ... } , children ) = l 

 val () = loopTripCount ( state , env , fmil , cfg , labelDominanceInfo , header ) 

 val () = Vector.foreach ( children , doLoop )  in ( ) end  

 val () = Vector.foreach ( getLoops ls , doLoop ) 

 val tcs = tcStateFinish state 

 val LS x = ls 

 val r = LS { config=# config x , si=# si x , entry=# entry x , loops=# loops x , blocksNotInLoops=# blocksNotInLoops x , allNodes=# allNodes x , exits=# exits x , preheaders=# preheaders x , inductionVars=# inductionVars x , tripCounts=SOME tcs , binderLocations=# binderLocations x }  in r end  

 fun genTripCounts' ( fmil , cfg , dt ) ls = genTripCounts ( ls , fmil , cfg , dt )  

 fun allTripCounts ( LS { tripCounts , ... } ) = ( case tripCounts of SOME tc => tc| NONE => fail ( "allTripCounts" , "trip counts have not been generated" ) )  

 fun getTripCount ( ls , h ) = LD.lookup ( allTripCounts ls , h )  

 val genBinderLocations : t -> t = fn ls => let 

 val appInstr : M.label -> ( M.instruction * ( M.label VD.t ) ) -> ( M.label VD.t ) = fn header => fn ( instr , dict ) => let 

 val M.I { dests , ... } = instr 

 val destsMap = Vector.map ( dests , fn x => ( x , header ) ) 

 val dict' = VD.insertAll ( dict , Vector.toList destsMap )  in dict' end 

 val appBlock : M.label -> ( M.block * ( M.label VD.t ) ) -> ( M.label VD.t ) = fn header => fn ( block , dict ) => let 

 val M.B { parameters , instructions , ... } = block 

 val paramsMap = Vector.map ( parameters , fn x => ( x , header ) ) 

 val dict' = VD.insertAll ( dict , Vector.toList paramsMap ) 

 val dict'' = foldl ( appInstr header ) dict' ( Vector.toList instructions )  in dict'' end 

 val appLoop : loop * ( M.label VD.t ) -> ( M.label VD.t ) = fn ( loop , dict ) => let 

 val L { header , blocks } = loop 

 val dict' = foldl ( appBlock header ) dict ( LD.range blocks )  in dict' end 

 val rec appTree : loopTree * ( M.label VD.t ) -> ( M.label VD.t ) = fn ( loopTree , dict ) => let 

 val ( Tree.T ( node , children ) ) = loopTree  in if ( Vector.isEmpty children ) then appLoop ( node , dict ) else let 

 val dict' = appLoop ( node , dict ) 

 val dict'' = Vector.fold ( children , dict' , appTree )  in dict'' end end 

 val LS { config , si , entry , loops , blocksNotInLoops , allNodes , exits , preheaders , inductionVars , tripCounts , ... } = ls 

 val dict = VD.empty 

 val dict' = Vector.fold ( loops , dict , appTree )  in LS { config=config , si=si , entry=entry , loops=loops , blocksNotInLoops=blocksNotInLoops , allNodes=allNodes , exits=exits , preheaders=preheaders , inductionVars=inductionVars , tripCounts=tripCounts , binderLocations=dict' } end 

 val getBinderLocations : t -> ( Mil.label Identifier.VariableDict.t ) = fn ( LS { binderLocations , ... } ) => binderLocations 

 fun layoutInductionVariable ( c , si , iv ) = let 

 val layoutBase = fn { variable , init , step } => L.seq [ MilLayout.layoutVariable ( c , si , variable ) , L.str " = " , Rat.layout step , L.str "*# + " , MilLayout.layoutOperand ( c , si , init ) ] 

 val i = ( case iv of BIV base => layoutBase base| DIV { variable , base , scale , offset } => let 

 val vb = MilLayout.layoutVariable ( c , si , # variable base ) 

 val vd = MilLayout.layoutVariable ( c , si , variable ) 

 val base = layoutBase base 

 val dvd = L.seq [ Rat.layout scale , L.str "*" , vb , L.str " + " , Rat.layout offset ] 

 val l = L.seq [ vd , L.str " = " , dvd , L.str " where " , base ]  in l end )  in i end  

 fun layoutTripCount ( cg , si , tc ) = let 

 val TC { block , cond , flip1 , comparison , flip2 , init = ( m , i , c ) , step , bound } = tc 

 val iv = L.seq [ Rat.layout step , L.str "*# + " , Rat.layout m , L.str "*" , MilLayout.layoutOperand ( cg , si , i ) , L.str " + " , Rat.layout c ] 

 val bound = MilLayout.layoutOperand ( cg , si , bound ) 

 val ( o1 , o2 ) = if flip2 then ( bound , iv ) else ( iv , bound ) 

 val cmp = PU.Layout.compareOp ( cg , comparison ) 

 val cmp = if flip1 then L.seq [ L.str "not" , cmp ] else cmp 

 val test = L.seq [ cmp , LU.parenSeq [ o1 , o2 ] ] 

 val i = L.seq [ MilLayout.layoutVariable ( cg , si , cond ) , L.str " = " , test ]  in i end  

 fun layoutLoopPreheader ( c , si , pres , header ) = let 

 val l = ( case LD.lookup ( pres , header ) of SOME blk => L.seq [ L.str "Pre-header: " , I.layoutLabel blk ]| NONE => L.str "Pre-header could not be generated." )  in l end  

 fun layoutLoopInductionVars ( c , si , ivs , header ) = let 

 val ivs = ( case LD.lookup ( ivs , header ) of SOME ivs => ivs| NONE => [ ] ) 

 fun doOne iv = layoutInductionVariable ( c , si , iv )  

 val l = L.align ( List.map ( ivs , doOne ) ) 

 val l = L.align [ L.str "Induction variables: " , LU.indent l ]  in l end  

 fun layoutLoopTripCount ( c , si , tcs , header ) = let 

 val l = ( case LD.lookup ( tcs , header ) of SOME tc => L.seq [ L.str "Trip count: " , layoutTripCount ( c , si , tc ) ]| NONE => L.str "No trip count for loop." )  in l end  

 fun layoutBlocks ( c , si , blks ) = let 

 val blks = LD.toList blks 

 fun doOne ( l , _ ) = I.layoutLabel l  

 val l = LU.sequence ( "{" , "}" , "," ) ( List.map ( blks , doOne ) )  in l end  

 fun layoutLoopExits ( c , si , exits , header ) = let 

 val exits = ( case LD.lookup ( exits , header ) of SOME exits => exits| NONE => LS.empty ) 

 val l = LS.layout ( exits , I.layoutLabel ) 

 val l = L.seq [ L.str "Loop exits: " , l ]  in l end  

 fun layoutLoops ( c , si , exits , preheaders , inductionVars , tripCounts , ls ) = let 

 val doPre = ( case preheaders of SOME pres => ( fn header => SOME ( layoutLoopPreheader ( c , si , pres , header ) ) )| NONE => ( fn header => NONE ) ) 

 val doIvs = ( case inductionVars of SOME ivs => ( fn header => SOME ( layoutLoopInductionVars ( c , si , ivs , header ) ) )| NONE => ( fn header => NONE ) ) 

 val doTcs = ( case tripCounts of SOME tcs => ( fn header => SOME ( layoutLoopTripCount ( c , si , tcs , header ) ) )| NONE => ( fn header => NONE ) ) 

 val doExits = ( case exits of SOME exits => ( fn header => SOME ( layoutLoopExits ( c , si , exits , header ) ) )| NONE => ( fn header => NONE ) ) 

 fun layout ls = let 

 val Tree.T ( L { header , blocks , ... } , children ) = ls 

 val l0 = SOME ( L.seq [ L.str "Header: " , I.layoutLabel header ] ) 

 val l1 = doPre header 

 val l2 = doIvs header 

 val l3 = doTcs header 

 val l4 = doExits header 

 val l5 = let 

 val l5 = layoutBlocks ( c , si , blocks ) 

 val l5 = L.mayAlign [ L.str "Blocks:" , LU.indent l5 ]  in SOME l5 end 

 val l6 = if Vector.length children > 0 then let 

 val l6 = Vector.toListMap ( children , layout ) 

 val l6 = L.align [ L.str "Loops:" , LU.indent ( L.align l6 ) ]  in SOME l6 end else NONE 

 val ll = Utils.List.concatOption [ l0 , l1 , l2 , l3 , l4 , l5 , l6 ] 

 val l = L.align ll  in l end  

 val l = layout ls  in l end  

 fun layout ( c , si , LS { entry , loops , blocksNotInLoops , exits , preheaders , inductionVars , tripCounts , ... } ) = let 

 val l1 = L.seq [ L.str "Entry: " , I.layoutLabel entry ] 

 val l2' = layoutBlocks ( c , si , blocksNotInLoops ) 

 val l2 = L.mayAlign [ L.str "Blocks:" , LU.indent l2' ] 

 val l3' = Vector.toListMap ( loops , fn ls => layoutLoops ( c , si , exits , preheaders , inductionVars , tripCounts , ls ) ) 

 val l3 = L.align [ L.str "Loops:" , LU.indent ( L.align l3' ) ] 

 val l = L.align [ l1 , l2 , l3 ]  in l end   end ; 

