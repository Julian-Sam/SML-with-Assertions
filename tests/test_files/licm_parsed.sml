signature MIL_LICM = sig val pass : ( BothMil.t , BothMil.t ) Pass.t  end structure MilLicm :> MIL_LICM = struct 

 val passname = "MilLicm" 

 val stats = [ ( "licm" , "loop-invariant instructions moved" ) ] structure L = Layout structure LU = LayoutUtils structure PD = PassData structure I = Identifier structure IM = Identifier.Manager structure VD = Identifier.VariableDict structure VS = Identifier.VariableSet structure LD = Identifier.LabelDict structure LS = Identifier.LabelSet structure M = Mil structure MU = MilUtils structure MUI = MU.Instruction structure FV = MilFreeVars structure Cfg = MilCfg structure DomInfo = Cfg.LabelDominance structure Loop = MilLoop 

 datatype state = S of { stm : M.symbolTableManager , pd : PD.t } 

 fun stateMk ( stm , pd ) = S { stm=stm , pd=pd }  

 fun stateGetStm ( S { stm , ... } ) = stm  

 fun getSi s = I.SymbolInfo.SiManager ( stateGetStm s )  

 fun getPd ( S { pd , ... } ) = pd  

 fun clickN ( state , stat , n ) = PD.clickN ( getPd state , stat , n )  

 datatype env = E of { config : Config.t } 

 fun envMk c = E { config=c }  

 val ( ( _ , getConfig ) ) = FunctionalUpdate.mk1 ( fn ( E { config } ) => ( config ) , fn ( config ) => E { config=config } ) structure Chat = ChatF ( struct 

 type env = env 

 val extract = getConfig 

 val name = passname 

 val indent = 2  end ) 

 datatype loopStructureInfo = Lsi of { di : DomInfo.t , ls : Loop.t , iphs : Mil.label LD.t } 

 fun getDi ( Lsi { di , ... } ) = di  

 fun getLs ( Lsi { ls , ... } ) = ls  

 fun getExits ( lsi , h ) = Loop.getExits ( getLs lsi , h )  

 fun getPreheader ( lsi , h ) = Loop.getPreheader ( getLs lsi , h )  

 fun getHeader ( Lsi { iphs , ... } , ph ) = LD.lookup ( iphs , ph )  

 fun dominates ( lsi , l1 , l2 ) = let 

 val l1 = Utils.Option.get ( getHeader ( lsi , l1 ) , l1 ) 

 val d = DomInfo.dominates ( getDi lsi , l1 , l2 )  in d end  

 fun dominatesSet ( lsi , l , ls ) = LS.forall ( ls , fn l' => dominates ( lsi , l , l' ) )  

 val ( debugPassD , debugPass ) = Config.Debug.mk ( passname , "debug the Mil loop-invariant code motion pass" ) 

 fun debugDo ( env , f ) = if Config.debug andalso debugPass ( getConfig env ) then f ( ) else ( )  

 fun printVS ( banner , s ) = let 

 val () = print banner 

 val () = LU.printLayout ( VS.layout ( s , Identifier.layoutVariable' ) )  in ( ) end  

 fun varsDefInBlock ( state , env , M.B { parameters , instructions , transfer } , s ) : VS.t = let 

 val s = case transfer of M.TInterProc { ret = M.RNormal { rets , ... } , ... } => Vector.fold ( rets , s , VS.insert o Utils.flip2 )| _ => s 

 fun addI ( M.I { dests , ... } , res ) = Vector.fold ( dests , res , VS.insert o Utils.flip2 )  

 val s = Vector.fold ( instructions , s , addI ) 

 val s = Vector.fold ( parameters , s , VS.insert o Utils.flip2 )  in s end  

 fun varsDefInLoopTree ( state , env , lt ) : VS.t = let 

 fun help1 ( _ , b , s ) = varsDefInBlock ( state , env , b , s )  

 fun help2 ( l , s ) = let 

 val Loop.L { header , blocks , ... } = l 

 val s = LD.fold ( blocks , s , help1 )  in s end  

 val s = Tree.foldPre ( lt , VS.empty , help2 )  in s end  

 fun topoSortInstructions ( state , env , invInstrs ) = let 

 val config = getConfig env 

 val help = fn ( i , l ) => Vector.fold ( MUI.dests i , l , fn ( v , l ) => ( v , i ) :: l ) 

 val vis = Vector.fold ( invInstrs , [ ] , help ) 

 val components = I.variableTopoSort ( vis , fn ( _ , i ) => FV.instruction ( config , i ) ) 

 fun extract c = case c of [ ( _ , i ) ] => i| _ => Fail.fail ( "MilLicm" , "topoSortInstructions" , "recursive instructions" )  

 val is = Vector.fromListMap ( components , extract )  in is end  

 fun canLicm ( state , env , i ) = Effect.subset ( MUI.fx ( getConfig env , i ) , Effect.InitReadS )  

 fun licmTree ( state , env , lsi , oph , lt ) = let 

 val Tree.T ( root , children ) = lt 

 val Loop.L { header , blocks , ... } = root 

 val () = debugDo ( env , fn () => print ( "Doing loop tree: " ^ I.labelString header ^ "\n" ) ) 

 val ( children , blocks ) = licmForest ( state , env , lsi , blocks , children ) 

 val l = Loop.L { header=header , blocks=blocks } 

 val ( l , oph ) = case oph of NONE => let 

 val () = Chat.warn0 ( env , "Loop has no preheader: " ^ I.labelString header )  in ( l , NONE ) end| SOME ( phl , phb ) => let 

 val ( l , phb ) = licmTreeA ( state , env , lsi , phb , l , children )  in ( l , SOME ( phl , phb ) ) end 

 val lt = Tree.T ( l , children )  in ( lt , oph ) end and licmTreeA ( state , env , lsi , phb , l as Loop.L { header , blocks } , children ) = let 

 val exits = getExits ( lsi , header ) 

 fun checkOne ( l , b , blks ) = if dominatesSet ( lsi , l , exits ) then ( l , b ) :: blks else blks  

 val blocksDominatingExits = LD.fold ( blocks , [ ] , checkOne ) 

 val varsDefInLoop = varsDefInLoopTree ( state , env , Tree.T ( l , children ) ) 

 val () = debugDo ( env , fn () => printVS ( "VarsDefInLoop" , varsDefInLoop ) ) 

 fun instructionInvariants ( i , inv ) = case Utils.Option.fromVector ( MUI.dests i ) of NONE => inv| SOME NONE => inv| SOME ( SOME v ) => let 

 val usesFromLoop = VS.intersection ( FV.instruction ( getConfig env , i ) , varsDefInLoop )  in if VS.isSubset ( usesFromLoop , inv ) andalso canLicm ( state , env , i ) then VS.insert ( inv , v ) else inv end  

 fun blockInvariants ( ( _ , b ) , inv ) = Vector.fold ( MU.Block.instructions b , inv , instructionInvariants )  

 fun calcLoopInvariants inv = let 

 val res = List.fold ( blocksDominatingExits , inv , blockInvariants )  in if VS.equal ( res , inv ) then res else calcLoopInvariants res end  

 val loopInvariants = calcLoopInvariants VS.empty 

 val () = debugDo ( env , fn () => printVS ( "Loop invariants:" , loopInvariants ) ) 

 fun isInvInstr i = ( case Utils.Option.fromVector ( MUI.dests i ) of SOME ( SOME v ) => VS.member ( loopInvariants , v )| _ => false )  

 fun removeFromBlock ( l , b , invis ) = let 

 val M.B { parameters , instructions , transfer } = b 

 val { yes , no } = Vector.partition ( instructions , isInvInstr ) 

 val b = M.B { parameters=parameters , instructions=no , transfer=transfer }  in ( b , yes :: invis ) end  

 val ( blocks , invis ) = LD.mapFold ( blocks , [ ] , removeFromBlock ) 

 val invInstrs = Vector.concat invis 

 val () = clickN ( state , "licm" , Vector.length invInstrs ) 

 fun printInvInstrs is () = let 

 val () = print ( "LICM: invariant instructions being moved: (" ^ Int.toString ( Vector.length is ) ^ ")\n" ) 

 fun prnOne i = LU.printLayout ( MilLayout.layoutInstruction ( getConfig env , getSi state , i ) )  

 val () = Vector.foreach ( is , prnOne )  in ( ) end  

 val invInstrs = topoSortInstructions ( state , env , invInstrs ) 

 val () = debugDo ( env , printInvInstrs invInstrs ) 

 val M.B { parameters , instructions , transfer } = phb 

 val instructions = Vector.concat [ instructions , invInstrs ] 

 val phb = M.B { parameters=parameters , instructions=instructions , transfer=transfer } 

 val l = Loop.L { header=header , blocks=blocks }  in ( l , phb ) end and licmForest ( state , env , lsi , pblks , loops ) = let 

 fun doOne ( lt , pblks ) = let 

 val Tree.T ( Loop.L { header , ... } , _ ) = lt 

 val oph = getPreheader ( lsi , header ) 

 val oph = Option.map ( oph , fn phl => ( phl , Option.valOf ( LD.lookup ( pblks , phl ) ) ) ) 

 val ( lt , oph ) = licmTree ( state , env , lsi , oph , lt ) 

 val pblks = case oph of NONE => pblks| SOME ( phl , phb ) => LD.insert ( pblks , phl , phb )  in ( lt , pblks ) end  

 val ( loops , pblks ) = Vector.mapAndFold ( loops , pblks , doOne )  in ( loops , pblks ) end  

 fun licmLoopStructure ( state , env , cfg , di , ls ) = let 

 val ls = Loop.addPreheaders ( ls , stateGetStm state ) 

 fun printPreheaders () = let 

 val () = print "LICM preheaders:\n" 

 fun prnOne ( l1 , l2 ) = L.seq [ I.layoutLabel l1 , L.str ": " , I.layoutLabel l2 ]  

 val () = LU.printLayout ( LD.layout ( Loop.getPreheaders ls , prnOne ) )  in ( ) end  

 val () = debugDo ( env , printPreheaders ) 

 val iphs = LD.fold ( Loop.getPreheaders ls , LD.empty , fn ( hl , phl , iphs ) => LD.insert ( iphs , phl , hl ) ) 

 val lsi = Lsi { di=di , ls=ls , iphs=iphs } 

 val entry = Loop.getEntry ls 

 val loops = Loop.getLoops ls 

 val blks = Loop.getBlocksNotInLoops ls 

 val ( loops , blks ) = licmForest ( state , env , lsi , blks , loops ) 

 val ls = Loop.fromLoops ( getConfig env , getSi state , { entry=entry , loops=loops , blocksNotInLoops=blks } )  in ls end  

 fun printLoops ( state , env , fname , code , cfg , lbdomtree , ls ) = let 

 val config = getConfig env 

 val sti = getSi state 

 val l = L.align [ L.seq [ L.str "Function " , MilLayout.layoutVariable ( config , sti , fname ) ] , L.str "Code is:" , LU.indent ( MilLayout.layoutCode ( config , sti , code ) ) ] 

 val () = LU.printLayout l 

 val () = LU.printLayout ( Loop.layout ( config , sti , ls ) ) 

 fun doOne ( l , ls ) = L.mayAlign [ L.seq [ I.layoutLabel l , L.str ":" ] , LU.sequence ( "{" , "}" , "," ) ( List.map ( LS.toList ls , I.layoutLabel ) ) ]  

 val le' = List.map ( LD.toList ( Loop.allExits ls ) , doOne ) 

 val le = L.align [ L.str "Exits:" , LU.indent ( L.align le' ) ] 

 val () = LU.printLayout le  in ( ) end  

 val ( showLoopsD , showLoops ) = Config.Debug.mk ( passname ^ ":show-loops" , "show loops in loop-invariant code motion" ) 

 fun processCode ( state , env , fname , code ) = let 

 val config = getConfig env 

 val si = getSi state 

 val M.F { fx , escapes , recursive , cc , args , rtyps , body } = code 

 val cfg = Cfg.build ( config , si , body ) 

 val lbdt = Cfg.getLabelBlockDomTree cfg 

 val ls = Loop.build ( config , si , cfg , lbdt ) 

 val ls = Loop.genAllNodes ls 

 val ls = Loop.genExits ls 

 val () = if showLoops ( getConfig env ) then printLoops ( state , env , fname , code , cfg , lbdt , ls ) else ( ) 

 val di = DomInfo.new ( Tree.map ( lbdt , # 1 ) ) 

 val ls = licmLoopStructure ( state , env , cfg , di , ls ) 

 val body = Loop.unbuild ls 

 val res = M.F { fx=fx , escapes=escapes , recursive=recursive , cc=cc , args=args , rtyps=rtyps , body=body }  in res end  

 fun doGlobals ( state , env , gs ) = let 

 fun licmGlobal ( v , g ) = case g of M.GCode code => M.GCode ( processCode ( state , env , v , code ) )| _ => g  

 val gs = VD.map ( gs , licmGlobal )  in gs end  

 fun program ( mil , pd ) = let 

 val M.P { includes , externs , globals , symbolTable , entry } = mil 

 val stm = IM.fromExistingAll symbolTable 

 val state = stateMk ( stm , pd ) 

 val env = envMk ( PD.getConfig pd ) 

 val globals = doGlobals ( state , env , globals ) 

 val st = IM.finish stm 

 val mil = M.P { includes=includes , externs=externs , globals=globals , symbolTable=st , entry=entry } 

 fun finalPrint () = let 

 val () = print ">printing FINAL mil\n" 

 val () = LU.printLayout ( MilLayout.layout ( getConfig env , mil ) ) 

 val () = print "<printing FINAL mil\n"  in ( ) end  

 val () = PD.report ( pd , passname )  in mil end  

 val description = { name=passname , description="Loop-invariant code motion" , inIr=BothMil.irHelpers , outIr=BothMil.irHelpers , mustBeAfter=[ ] , stats=stats } 

 val associates = { controls=[ ] , debugs=[ debugPassD , showLoopsD ] , features=[ ] , subPasses=[ ] } 

 val pass = Pass.mkOptPass ( description , associates , BothMil.mkMilPass program )  end 

