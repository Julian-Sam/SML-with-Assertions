signature IMIL_BLOCK = sig include IMIL_PUBLIC_TYPES val build : t * iFunc * ( Mil.label * Mil.block ) -> iBlock val delete : t * iBlock -> unit val getIFunc : t * iBlock -> iFunc val getTransfer : t * iBlock -> iInstr val getTransfer' : t * iBlock -> Mil.transfer val getLabel : t * iBlock -> iInstr val getLabel' : t * iBlock -> Mil.label * Mil.variable Vector.t val getParameters : t * iBlock -> Mil.variable Vector.t val getFirst : t * iBlock -> iInstr option val getUsedBy : t * iBlock -> item Vector.t val freeVars : t * iBlock -> Identifier.VariableSet.t val freeVars' : t * iBlock -> Mil.variable list val preds : t * iBlock -> iBlock list val succs : t * iBlock -> iBlock list val inEdges : t * iBlock -> ( iBlock * iBlock ) list val outEdges : t * iBlock -> ( iBlock * iBlock ) list val merge : t * iBlock * iBlock -> unit val prepend : t * Mil.instruction * iBlock -> iInstr val prepend' : t * iInstr * iBlock -> unit val append : t * iBlock * Mil.instruction -> iInstr val append' : t * iBlock * iInstr -> unit val replaceTransfer : t * iBlock * Mil.transfer -> unit val replaceLabel : t * iBlock * ( Mil.label * ( Mil.variable Vector.t ) ) -> unit val addNewPredecessor : t * iBlock -> iBlock val makeSinglePred : t * iBlock -> iBlock val splitEdge : t * ( iBlock * iBlock ) -> iBlock option val splitInEdges : t * iBlock -> iBlock list * bool val splitOutEdges : t * iBlock -> iBlock list * bool val isStart : t * iBlock -> bool val isExit : t * iBlock -> bool val isEmpty : t * iBlock -> bool val isDead : t * iBlock -> bool val layout : t * iBlock -> Layout.t  end structure IMilBlock : sig include IMIL_BLOCK val new : t * iFunc * Mil.label -> iBlock val init : t * iBlock * ( Mil.label * Mil.block ) -> unit val unBuild : iBlock -> ( Mil.label * Mil.block ) option val replaceTransfer' : t * iBlock * mInstr -> unit val replaceLabel' : t * iBlock * mInstr -> unit  end = struct  open IMilPublicTypes 

 val fail = fn ( f , s ) => Fail.fail ( "block.sml" , f , s ) structure M = Mil structure L = Layout structure LU = LayoutUtils structure IMT = IMilTypes structure IVD = IMT.IVD structure ILD = IMT.ILD structure VS = IMT.VS structure FV = IMilCommon.FV structure Def = IMilDef structure Var = IMilVar structure IML = IMilLayout structure Graph = IMT.Graph structure Instr = IMilInstr 

 val getIFunc = fn ( p , b ) => IMT.iBlockGetIFunc b 

 val preds = fn ( p , b ) => IMT.iBlockGetPredIBlocks b 

 val succs = fn ( p , b ) => IMT.iBlockGetSuccIBlocks b 

 val inEdges = fn ( p , b ) => IMT.iBlockGetIBlockInEdges b 

 val outEdges = fn ( p , b ) => IMT.iBlockGetIBlockOutEdges b 

 val getTransfer = fn ( p , b ) => IMT.iBlockGetTrans b 

 val getLabel = fn ( p , b ) => IMT.iBlockGetLabel b 

 val getFirst = fn ( p , b ) => let 

 val code = IMT.iBlockGetCode b 

 val res = Option.map ( DList.first code , DList.getVal )  in res end 

 val getLabel' = fn ( p , b ) => case IMT.iInstrGetMil ( getLabel ( p , b ) ) of IMT.MLabel x => x| x => fail ( "getLabel'" , "bad label inst" ) 

 val getTransfer' = fn ( p , b ) => case IMT.iInstrGetMil ( getTransfer ( p , b ) ) of IMT.MTransfer x => x| _ => fail ( "getTransfer'" , "bad transfer inst" ) 

 val getParameters = fn ( p , b ) => # 2 ( getLabel' ( p , b ) ) 

 val setNode = fn ( p , b , n ) => IMT.iBlockSetNode ( b , n ) 

 val setIFunc = fn ( p , b , iFunc ) => IMT.iBlockSetIFunc ( b , iFunc ) 

 val delete = fn ( p , b ) => let 

 val IMT.B { label , code , trans , iFunc , node , ... } = IMT.iBlockGetIBlock' b 

 val () = Graph.deleteNode ( IMT.iFuncGetCfg iFunc , node ) 

 val () = ( case Instr.getMil ( p , label ) of IMT.MLabel ( l , _ ) => ILD.remove ( IMT.iFuncGetIBlocks iFunc , l )| _ => ( ) ) 

 val () = Instr.delete ( p , trans ) 

 val () = DList.foreach ( code , fn i => Instr.delete ( p , i ) ) 

 val () = Instr.delete ( p , label ) 

 val () = IMT.iBlockSetCode ( b , DList.empty ( ) )  in ( ) end 

 val new' = fn ( p , iFunc , l , label , trans ) => let 

 val b = IMT.iBlockNewUninitialized ( ) 

 val node = Graph.newNode ( IMT.iFuncGetCfg iFunc , SOME b ) 

 val id = IMT.nextId p 

 val b' = IMT.B { id=id , label=label , code=DList.empty ( ) , trans=trans , iFunc=iFunc , node=node } 

 val () = IMT.iBlockSetIBlock' ( b , b' ) 

 val () = Instr.setLoc ( p , label , b , NONE ) 

 val () = Instr.setLoc ( p , trans , b , NONE ) 

 val () = if l = IMT.iFuncGetStart iFunc then ignore ( Graph.addEdge ( IMT.iFuncGetCfg iFunc , IMT.iFuncGetEntry iFunc , node , ( ) ) ) else ( ) 

 val () = ILD.insert ( IMT.iFuncGetIBlocks iFunc , l , b )  in b end 

 val new = fn ( p , cfg , l ) => new' ( p , cfg , l , Instr.new p , Instr.new p ) 

 val unBuild = fn b => let 

 val code = DList.toList ( IMT.iBlockGetCode b ) 

 val instrs = List.keepAllMap ( code , Instr.toInstruction ) 

 val res = case ( Instr.toLabel ( IMT.iBlockGetLabel b ) , Instr.toTransfer ( IMT.iBlockGetTrans b ) , instrs ) of ( NONE , NONE , [ ] ) => NONE| ( SOME ( label , args ) , SOME t , _ ) => let 

 val block = M.B { parameters=args , instructions=Vector.fromList instrs , transfer=t }  in SOME ( label , block ) end| _ => fail ( "unBuild" , "Partially dead or mal-formed block!" )  in res end local 

 val add = fn adder => fn ( p , block , instr ) => let 

 val code = IMT.iBlockGetCode block 

 val c = adder ( code , instr ) 

 val () = Instr.setLoc ( p , instr , block , SOME c )  in ( ) end 

 val addFirst = add DList.insert 

 val addLast = add DList.insertLast  in 

 val prepend' = fn ( p , i , b ) => addFirst ( p , b , i ) 

 val prepend = fn ( p , mi , b ) => let 

 val m = IMT.MInstr mi 

 val i = Instr.new' ( p , m ) 

 val () = prepend' ( p , i , b )  in i end 

 val append' = fn ( p , b , i ) => addLast ( p , b , i ) 

 val append = fn ( p , b , mi ) => let 

 val m = IMT.MInstr mi 

 val i = Instr.new' ( p , m ) 

 val () = append' ( p , b , i )  in i end  end 

 val replaceTransfer' = fn ( p , b , mi ) => Instr.replaceMil ( p , getTransfer ( p , b ) , mi ) 

 val replaceLabel' = fn ( p , b , mi ) => Instr.replaceMil ( p , getLabel ( p , b ) , mi ) 

 val replaceTransfer = fn ( p , b , t ) => replaceTransfer' ( p , b , IMT.MTransfer t ) 

 val replaceLabel = fn ( p , b , lv ) => replaceLabel' ( p , b , IMT.MLabel lv ) 

 val init = fn ( p , block , ( l , b ) ) => let 

 val M.B { parameters , instructions , transfer } = b 

 val () = Vector.foreachr ( instructions , fn mi => let 

 val i = prepend ( p , mi , block )  in ( ) end ) 

 val () = replaceTransfer ( p , block , transfer ) 

 val () = replaceLabel ( p , block , ( l , parameters ) )  in ( ) end 

 val build = fn ( p , iFunc , ( l , b ) ) => let 

 val ib = new ( p , iFunc , l ) 

 val () = init ( p , ib , ( l , b ) )  in ib end 

 val merge = fn ( p , b1 , b2 ) => let 

 val () = if b1 = b2 then fail ( "merge" , "Tried to merge block with itself" ) else ( ) 

 val iTrans = getTransfer ( p , b2 ) 

 val trans = Instr.getMil ( p , iTrans ) 

 val () = Instr.delete ( p , iTrans ) 

 val () = replaceTransfer' ( p , b1 , trans ) 

 val first = getFirst ( p , b2 ) 

 val rec loop = fn io => ( case io of SOME i => let 

 val io = Instr.next ( p , i ) 

 val () = append' ( p , b1 , i )  in loop io end| NONE => ( ) ) 

 val () = loop first 

 val () = delete ( p , b2 )  in ( ) end 

 val addNewPredecessor' = fn ( p , b ) => let 

 val nl = Var.labelFresh p 

 val iFunc = IMT.iBlockGetIFunc b 

 val nb = new ( p , iFunc , nl ) 

 val ( l , vs ) = getLabel' ( p , b ) 

 val nvs = Vector.map ( vs , fn v => Var.clone ( p , v ) ) 

 val () = replaceLabel ( p , nb , ( nl , nvs ) ) 

 val args = Vector.map ( nvs , M.SVariable ) 

 val nt = M.TGoto ( M.T { block=l , arguments=args } ) 

 val () = replaceTransfer ( p , nb , nt )  in ( nl , nb ) end 

 val addNewPredecessor = fn ( p , b ) => # 2 ( addNewPredecessor' ( p , b ) ) 

 val splitEdge = fn ( p , ( b1 , b2 ) ) => Try.try ( fn () => let 

 val ( l2 , _ ) = getLabel' ( p , b2 ) 

 val t = getTransfer' ( p , b1 ) 

 val isTarget = fn M.T { block , arguments } => block = l2 

 val doTarget = fn ( l3 , M.T { block , arguments } ) => M.T { block=l3 , arguments=arguments } 

 val succeed = fn f => let 

 val ( l3 , b3 ) = addNewPredecessor' ( p , b2 ) 

 val t = f l3 

 val () = replaceTransfer ( p , b1 , t )  in b3 end 

 val doSwitch = ( fn ( l3 , { select , on = a , cases = v , default = defo } ) => let 

 val help = fn ( arg as ( ( a , tg ) , done ) ) => if done orelse not ( isTarget tg ) then arg else ( ( a , doTarget ( l3 , tg ) ) , true ) 

 val def = fn d => case d of SOME tg => SOME ( doTarget ( l3 , tg ) )| NONE => fail ( "retargetOutEdge" , "Switch" )  in ( case Vector.mapAndFold ( v , false , help ) of ( v , true ) => M.TCase { select=select , on=a , cases=v , default=defo }| ( _ , false ) => M.TCase { select=select , on=a , cases=v , default=def defo } ) end ) 

 val b3 = case t of M.TGoto tg => succeed ( fn l3 => M.TGoto ( doTarget ( l3 , tg ) ) )| M.TReturn _ => fail ( "retargetOutEdge" , "Return" )| M.TInterProc { callee , ret , fx } => ( case ret of M.RNormal { rets , block , cuts } => let 

 val () = Try.require ( block = l2 ) 

 val r = succeed ( fn l3 => let 

 val ret = M.RNormal { rets=rets , block=l3 , cuts=cuts }  in M.TInterProc { callee=callee , ret=ret , fx=fx } end )  in r end| M.RTail _ => fail ( "retargetOutEdge" , "TailCall" ) )| M.TCase sw => succeed ( fn l3 => doSwitch ( l3 , sw ) )| M.TCut _ => Try.fail ( )| M.THalt _ => fail ( "retargetOutEdge" , "Halt" )  in b3 end ) 

 val splitEdges = fn ( p , es ) => let 

 val failed = ref false 

 val help = fn e => let 

 val bo = splitEdge ( p , e ) 

 val () = if isSome bo then ( ) else failed := true  in bo end 

 val bs = List.keepAllMap ( es , help )  in ( bs , ! failed ) end 

 val splitInEdges = fn ( p , b ) => let 

 val edges = IMT.iBlockGetIBlockInEdges b  in splitEdges ( p , edges ) end 

 val splitOutEdges = fn ( p , b ) => let 

 val edges = IMT.iBlockGetIBlockOutEdges b  in splitEdges ( p , edges ) end 

 val makeSinglePred = fn ( p , b ) => if List.length ( preds ( p , b ) ) = 1 then b else addNewPredecessor ( p , b ) 

 val freeVars = fn ( p , b ) => ( case unBuild b of SOME ( l , b ) => FV.block ( p , l , b )| NONE => VS.empty ) 

 val freeVars' = fn ( p , b ) => let 

 val s = freeVars ( p , b ) 

 val l = VS.toList s  in l end 

 val getUsedBy = fn ( p , b ) => let 

 val l = freeVars' ( p , b ) 

 val defs = Vector.fromListMap ( l , fn v => Def.get ( p , v ) ) 

 val items = Def.defsToItems ( p , defs )  in items end 

 val isStart = fn ( p , b ) => let 

 val iFunc = IMT.iBlockGetIFunc b 

 val entry = IMT.iFuncGetEntry iFunc 

 val graph = IMT.iFuncGetCfg iFunc 

 val succs = IMT.nodeGetSuccIBlocks entry 

 val b = List.contains ( succs , b , op = )  in b end 

 val isExit = fn ( p , b ) => let 

 val iFunc = IMT.iBlockGetIFunc b 

 val exit = IMT.iFuncGetExit iFunc 

 val graph = IMT.iFuncGetCfg iFunc 

 val succs = IMT.iBlockGetSuccNodes b 

 val b = List.contains ( succs , exit , op = )  in b end 

 val isEmpty = fn ( p , b ) => let 

 val l = DList.toListUnordered ( IMT.iBlockGetCode b ) 

 val dead = fn i => ( case Instr.getMil ( p , i ) of IMT.MDead => true| _ => false ) 

 val empty = List.forall ( l , dead )  in empty end 

 val isDead = fn ( p , b ) => ( case Instr.getMil ( p , IMT.iBlockGetLabel b ) of IMT.MDead => true| _ => false ) 

 val layout = fn ( p , b ) => let 

 val lbl = Instr.layout ( p , IMT.iBlockGetLabel b ) 

 val code = DList.toListMap ( IMT.iBlockGetCode b , ( fn i => Instr.layout ( p , i ) ) ) 

 val t = Instr.layout ( p , IMT.iBlockGetTrans b )  in L.align [ lbl , LU.indent ( L.align code ) , t ] end 

 val () = BackPatch.fill ( IML.iBlockH , layout )  end 

