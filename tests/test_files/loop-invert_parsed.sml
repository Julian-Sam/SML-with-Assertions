signature MIL_LOOP_INVERT = sig val pass : ( BothMil.t , BothMil.t ) Pass.t  end structure MilLoopInvert :> MIL_LOOP_INVERT = struct 

 val passname = "MilLoopInvert" 

 val stats = [ ( passname , "Loop Invert" ) ] 

 fun fail ( f , m ) = Fail.fail ( passname , f , m )  

 fun assert ( f , m , b ) = if b then fail ( f , m ) else ( )  structure M = Mil structure V = Vector structure O = Option structure UO = Utils.Option structure UF = Utils.Function structure PD = PassData structure L = Layout structure LU = LayoutUtils structure I = Identifier structure IM = Identifier.Manager structure LD = I.LabelDict structure VD = I.VariableDict structure LS = I.LabelSet structure VS = I.VariableSet structure MU = MilUtils structure ML = MilLayout structure MBV = MilBoundVars structure MFV = MilFreeVars structure MR = MilRename 

 val <- = Try.<- 

 val try = Try.try 

 val require = Try.require 

 fun fmap f v = case v of SOME x => SOME ( f x )| NONE => NONE  

 type blocks = M.block LD.t 

 type frontier = { blocks : LS.t , exits : bool } 

 type domTree = ( M.label * blocks * frontier ) Tree.t 

 val vempty = V.new0 

 val vmap = Tree.Seq.map 

 val vfold = Tree.Seq.fold 

 fun keepAll ( v , f ) = vfold ( v , [ ] , fn ( x , l ) => if f x then x :: l else l )  

 fun unionFrontiers ( m : frontier , n : frontier ) = { blocks=LS.union ( # blocks m , # blocks n ) , exits=( # exits m ) orelse ( # exits n ) }  

 fun treeChildren ( Tree.T ( _ , c ) : domTree ) = c  

 fun treeLabel ( Tree.T ( ( l , _ , _ ) , _ ) : domTree ) = l  

 fun treeBlocks ( Tree.T ( ( _ , b , _ ) , _ ) : domTree ) = b  

 fun treeFrontier ( Tree.T ( ( _ , _ , f ) , _ ) : domTree ) = f  

 fun treeChildrenLabels t = vmap ( treeChildren t , treeLabel )  

 fun treeContainsLabel t l = ( treeLabel t = l ) orelse V.exists ( treeChildren t , fn c => treeContainsLabel c l )  

 fun rename func ( config , blks , dict ) = LD.map ( blks , fn ( l , blk ) => # 2 ( func ( config , dict , l , blk ) ) )  

 val renameLabels = rename MR.Label.block 

 val renameVars = rename MR.Var.block 

 val renameBoth = rename MR.VarLabel.block 

 type stat = { total : int ref , inverted : int ref , aborted : int ref } structure Control = struct 

 fun default _ = 4  

 fun parse ( s : string ) = case Int.fromString s of NONE => NONE| SOME n => if n >= 0 then SOME n else NONE  

 fun description () = L.str ( "Max number of blocks in R1 (between loop header and " ^ "loop exit) blocks allowed during loop inversion. This controls " ^ "code duplication, and the default is " ^ Int.toString ( default ( ) ) ^ ".\n" )  

 val name = passname ^ ":max-r1-size" 

 val ( maxR1Size , getMaxR1Size ) = Config.Control.mk ( name , description , parse , default )  end structure Debug = struct 

 val ( debugPassD , debugPass ) = Config.Debug.mk ( passname , "debug the Mil loop invert" ) 

 fun isDebug config = Config.debug andalso debugPass config  

 fun when b f x = if b then f x else ( )  

 fun debug config = when ( isDebug config )  

 fun detailDebug config = when ( isDebug config andalso Config.verbose config )  

 fun layoutTree ( Tree.T ( ( label , blks , { blocks , exits } ) , children ) ) = L.align [ L.seq [ L.str "(" , I.layoutLabel label , L.str " => " , LU.sequence ( "{" , "}" , "," ) ( map I.layoutLabel ( LS.toList blocks ) ) , LU.layoutBool exits , L.str ")" ] , LU.indent ( L.align ( map layoutTree ( V.toList children ) ) ) ]  

 fun layoutBlocks ( config , sm , blks ) = let 

 val si = I.SymbolInfo.SiManager sm 

 fun layoutBlock ( l , b ) = ML.layoutBlock ( config , si , ( l , b ) )   in LU.sequence ( "" , "" , "\n" ) ( List.map ( LD.toList blks , layoutBlock ) ) end   end 

 fun collapse ( Tree.T ( ( label , blks , frontier ) , children ) ) : domTree = Tree.T ( ( label , vfold ( vmap ( children , treeBlocks o collapse ) , blks , fn ( b , d ) => LD.union ( b , d , # 3 ) ) , frontier ) , vempty ( ) )  

 val closeRegion : Config.t * M.symbolTableManager * M.label * blocks * I.variable V.t -> M.label * blocks * Rename.t = fn ( config , sm , entrylabel , blks , arguments ) => if V.isEmpty arguments then ( entrylabel , blks , Rename.none ) else let 

 val M.B { parameters , ... } = <- ( LD.lookup ( blks , entrylabel ) ) 

 val parameters = V.map ( parameters , fn v => IM.variableClone ( sm , v ) ) 

 val newarguments = V.map ( arguments , fn v => IM.variableClone ( sm , v ) ) 

 val newparameters = V.concat [ parameters , newarguments ] 

 val renamedict = V.fold ( V.zip ( arguments , newarguments ) , Rename.none , fn ( ( m , n ) , d ) => Rename.renameTo ( d , m , n ) ) 

 val blks = renameVars ( config , blks , renamedict ) 

 val trans = M.TGoto ( M.T { block=entrylabel , arguments=V.map ( parameters , M.SVariable ) } ) 

 val newentry = IM.labelFresh sm 

 val blks = LD.insert ( blks , newentry , M.B { parameters=newparameters , instructions=vempty ( ) , transfer=trans } )  in ( newentry , blks , renamedict ) end 

 val adjustTarget : M.label * M.label * blocks * I.variable V.t -> blocks option = fn ( targetlabel , newlabel , blks , args ) => let 

 fun replaceT ( target as M.T { block , arguments } ) = if block = targetlabel then M.T { block=newlabel , arguments=V.concat [ arguments , V.map ( args , M.SVariable ) ] } else target  

 fun replaceS ( { select , on , cases , default } ) = { select=select , on=on , cases=V.map ( cases , fn ( x , t ) => ( x , replaceT t ) ) , default=fmap replaceT default }  

 fun replaceTr ( M.TGoto t ) = SOME ( M.TGoto ( replaceT t ) )  
 | replaceTr ( M.TCase s ) = SOME ( M.TCase ( replaceS s ) )  
 | replaceTr ( x as M.TInterProc { ret = M.RNormal { block , ... } , ... } ) = if block = targetlabel then NONE else SOME x  
 | replaceTr ( x as M.TCut { cuts = M.C { targets , ... } , ... } ) = if LS.member ( targets , targetlabel ) then NONE else SOME x  
 | replaceTr x = SOME x   in try ( fn () => LD.map ( blks , fn ( _ , M.B { parameters , instructions , transfer } ) => M.B { parameters=parameters , instructions=instructions , transfer=<- ( replaceTr transfer ) } ) ) end 

 val cloneRegion : Config.t * M.symbolTableManager * M.label * blocks -> M.label * blocks = fn ( config , sm , entrylabel , blks ) => let 

 val vars = MBV.blocks ( config , blks ) 

 val vdict = VS.fold ( vars , Rename.none , fn ( v , d ) => Rename.renameTo ( d , v , IM.variableClone ( sm , v ) ) ) 

 val ldict = LD.map ( blks , fn _ => IM.labelFresh sm ) 

 val newentry = <- ( LD.lookup ( ldict , entrylabel ) ) 

 val newblks = renameBoth ( config , blks , ( vdict , ldict ) ) 

 val newblks = LD.fold ( newblks , blks , fn ( l , b , m ) => let 

 val l' = <- ( LD.lookup ( ldict , l ) )  in LD.insert ( m , l' , b ) end )  in ( newentry , newblks ) end 

 fun annotateFrontier ( Tree.T ( ( label , block ) , children ) ) : domTree = let 

 val children = vmap ( children , annotateFrontier ) 

 val labels = LS.fromVector ( vmap ( children , treeLabel ) ) 

 val successors = MU.Block.successors block 

 fun gather ( t , e ) = unionFrontiers ( treeFrontier t , e )  

 val childrenFrontiers = vfold ( children , successors , gather ) 

 val blocks = LS.difference ( # blocks childrenFrontiers , labels )  in Tree.T ( ( label , LD.singleton ( label , block ) , { blocks=blocks , exits=# exits childrenFrontiers } ) , children ) end  

 val tryInvertLoop : Config.t * M.symbolTableManager * domTree * stat -> domTree = fn ( config , sm , header , stat ) => let 

 val maxR1Size = Control.getMaxR1Size config 

 fun prints s = Debug.detailDebug config print s  

 fun printLayout ( m , l ) = LU.printLayout ( L.seq [ L.str m , l ] )  

 fun printList ( m , l ) = Debug.detailDebug config printLayout ( m , LU.sequence ( "{" , "}" , "," ) l )  

 fun printRegion ( m , r ) = Debug.detailDebug config printLayout ( m , Debug.layoutBlocks ( config , sm , r ) )  

 val headerlabel = treeLabel header 

 fun findRC () : domTree option = let 

 fun find ( internal , self ) = let 

 val children = treeChildren self 

 val labels = LS.fromVector ( treeChildrenLabels self ) 

 val internal = LS.union ( internal , labels ) 

 fun verify n = let 

 val f = treeFrontier n  in # exits f orelse ( not o LS.isEmpty o LS.difference ) ( # blocks f , internal ) end  

 fun found n = ( LS.isEmpty o LS.intersection ) ( # blocks ( treeFrontier n ) , internal )   in case keepAll ( children , verify ) of [ rc ] => if found rc then SOME rc else find ( internal , rc )| [ ] => NONE| _ => Try.fail ( ) end   in find ( LS.singleton headerlabel , header ) end  

 fun findR2 ( rc , max ) : domTree option = let 

 fun find ( internal , self ) = let 

 val children = treeChildren self 

 val labels = LS.fromVector ( treeChildrenLabels self ) 

 val internal = LS.union ( internal , labels ) 

 fun verify n = LS.member ( # blocks ( treeFrontier n ) , headerlabel )  

 val red = UO.dispatch ( rc , fn _ => UF.id , fn _ => UF.curry LS.intersection internal ) 

 fun found n = LS.isSubset ( red ( # blocks ( treeFrontier n ) ) , max )   in case keepAll ( children , verify ) of [ r2 ] => if found r2 then SOME r2 else find ( internal , r2 )| _ => NONE end   in find ( LS.singleton headerlabel , header ) end  

 fun adjustR2 ( r2 ) : domTree option = let 

 fun adjust ( internal , r2 ) : domTree option = let 

 val children = treeChildren r2 

 val labels = LS.fromVector ( treeChildrenLabels r2 ) 

 val internal = LS.union ( internal , labels ) 

 val blk = hd ( LD.range ( treeBlocks r2 ) ) 

 val successors = MU.Block.successors blk 

 fun verify n = LS.member ( # blocks ( treeFrontier n ) , headerlabel )   in if ( # exits successors orelse ( not o LS.isEmpty o LS.difference ) ( # blocks successors , internal ) ) then if V.size children = 0 then NONE else case keepAll ( children , verify ) of [ newR2 ] => adjust ( internal , newR2 )| _ => SOME r2 else SOME r2 end   in adjust ( LS.fromList [ headerlabel , treeLabel r2 ] , r2 ) end  

 fun findR1 ( exclude ) : ( domTree list ) option = let 

 fun find ( node , r1 ) = let 

 val l = treeLabel node 

 val children = treeChildren node  in if LS.member ( exclude , l ) then r1 else vfold ( children , node :: r1 , find ) end   in case find ( header , [ ] ) of [ ] => NONE| x => SOME x end  

 fun sanityCheck ( r1 ) : bool = not ( null r1 ) andalso List.forall ( r1 , fn n => LD.size ( treeBlocks n ) = 1 )  

 fun spineTo label : domTree list = let 

 fun find spine ( node , found ) = case ( null found , treeLabel node <> label ) of ( true , true ) => vfold ( treeChildren node , [ ] , find ( node :: spine ) )| ( false , _ ) => found| ( _ , false ) => spine   in find [ ] ( header , [ ] ) end  

 fun invertLoop ( r1 , r2 , rc ) : domTree option = try ( fn () => let 

 fun concatBlks t = List.fold ( t , LD.empty , fn ( x , y ) => LD.union ( x , y , # 3 ) )  

 val concatTreeBlks = concatBlks o map treeBlocks 

 fun boundedVarsInUse r = let 

 val label = treeLabel r 

 val toR = concatTreeBlks ( spineTo label ) 

 val r = treeBlocks ( collapse r ) 

 fun intersect ( s , t ) = VS.intersection ( MBV.blocks ( config , s ) , MFV.blocks ( config , t ) )  

 val rv = intersect ( toR , r ) 

 val _ = printList ( "Set of var: " , map I.layoutVariable' ( VS.toList rv ) )  in ( label , r , rv ) end  

 val r1 = concatTreeBlks r1 

 val r1size = LD.size r1 

 val _ = if r1size > maxR1Size then let 

 val _ = prints ( "Bail out due to R1 block size " ^ Int.toString r1size ^ " exceeds maxR1Size " ^ Int.toString maxR1Size ^ ".\n" ) 

 val _ = # aborted stat := ! ( # aborted stat ) + 1  in Try.fail ( ) end else ( ) 

 val ( r2l , r2 , r2v ) = boundedVarsInUse r2 

 fun inject ( l , r , v ) = ( SOME l , r , v )  

 val ( rcl , rc , rcv ) = UO.dispatch ( rc , inject o boundedVarsInUse , fn _ => ( NONE , LD.empty , VS.empty ) ) 

 val rvs = ( VS.toVector o VS.union ) ( r2v , rcv ) 

 val _ = prints "Closing R2\n" 

 val ( r2n , r2 , r2dict ) = closeRegion ( config , sm , r2l , r2 , rvs ) 

 val _ = printRegion ( "After closing: " , r2 ) 

 val r1 = <- ( adjustTarget ( r2l , r2n , r1 , rvs ) ) 

 val _ = printRegion ( "Adjusted R1: " , r1 ) 

 val ( r1 , r2 , rc ) = UO.dispatch ( rcl , fn rcl => let 

 val _ = prints "Closing RC\n" 

 val rcv = VS.toVector rcv 

 val ( rcn , rc , _ ) = closeRegion ( config , sm , rcl , rc , rcv ) 

 val _ = printRegion ( "After closing: " , rc ) 

 val r1 = <- ( adjustTarget ( rcl , rcn , r1 , rcv ) ) 

 val _ = printRegion ( "Adjusted R1: " , r1 ) 

 val rcv = V.map ( rcv , UF.curry Rename.use r2dict ) 

 val r2 = <- ( adjustTarget ( rcl , rcn , r2 , rcv ) ) 

 val _ = printRegion ( "Adjusted R2: " , r2 )  in ( r1 , r2 , rc ) end , fn () => ( r1 , r2 , rc ) ) 

 val _ = prints ( "Cloning R1 (" ^ Int.toString r1size ^ " blocks)\n" ) 

 val ( newlabel , r1 ) = cloneRegion ( config , sm , headerlabel , r1 ) 

 val _ = printRegion ( "Cloned R1: " , r1 ) 

 val r2 = renameLabels ( config , r2 , LD.fromList [ ( headerlabel , newlabel ) ] ) 

 val _ = printRegion ( "Adjusted R2: " , r2 )  in Tree.T ( ( headerlabel , concatBlks [ r1 , r2 , rc ] , treeFrontier header ) , vempty ( ) ) end )   in UO.out ( try ( fn () => let 

 val rc = findRC ( ) 

 val rclabels = map treeLabel ( UO.toList rc ) 

 val _ = printList ( "Found RC: " , map I.layoutLabel rclabels ) 

 val r2 = <- ( findR2 ( rc , LS.fromList ( headerlabel :: rclabels ) ) ) 

 val _ = map ( require o not o treeContainsLabel r2 ) rclabels 

 val _ = printList ( "Found R2: " , [ I.layoutLabel ( treeLabel r2 ) ] ) 

 val r2 = <- ( adjustR2 r2 ) 

 val r2label = treeLabel r2 

 val _ = printList ( "Adjusted R2: " , [ I.layoutLabel r2label ] ) 

 val r1 = <- ( findR1 ( LS.fromList ( r2label :: rclabels ) ) ) 

 val _ = require ( sanityCheck r1 ) 

 val _ = printList ( "Found R1: " , map ( I.layoutLabel o treeLabel ) r1 ) 

 val header = <- ( invertLoop ( r1 , r2 , rc ) ) 

 val _ = # inverted stat := ! ( # inverted stat ) + 1  in header end ) , fn () => collapse header ) end 

 fun doDom ( config , sm , self , stat ) : domTree = let 

 val Tree.T ( ( label , blk , frontier ) , children ) = self 

 val children = V.map ( children , fn c => doDom ( config , sm , c , stat ) ) 

 val self = Tree.T ( ( label , blk , frontier ) , children )  in if LS.member ( # blocks frontier , label ) then let 

 val _ = Debug.detailDebug config print ( "Loop header: " ^ I.labelString label ^ "\n" ) 

 val _ = Debug.detailDebug config LU.printLayout ( Debug.layoutTree self ) 

 val _ = # total stat := ! ( # total stat ) + 1 

 val b = tryInvertLoop ( config , sm , self , stat )  in b end else self end  

 fun doBody ( config , sm , body , stat ) = let 

 val si = I.SymbolInfo.SiManager sm 

 val M.CB { entry , blocks } = body 

 val cfg = MilCfg.build ( config , si , body ) 

 val dom = annotateFrontier ( MilCfg.getLabelBlockDomTree cfg ) 

 val dom = doDom ( config , sm , dom , stat ) 

 val blocks = treeBlocks ( collapse dom )  in M.CB { entry=entry , blocks=blocks } end  

 fun doGlobal ( config , M.P { includes , externs , globals = gs , symbolTable , entry } ) = let 

 val sm = I.Manager.fromExistingAll symbolTable 

 val stat = { total=ref 0 , inverted=ref 0 , aborted=ref 0 } 

 fun doCode ( M.F { fx , escapes , recursive , cc , args , rtyps , body } ) = M.F { fx=fx , escapes=escapes , recursive=recursive , cc=cc , args=args , rtyps=rtyps , body=doBody ( config , sm , body , stat ) }  

 val gs = VD.map ( gs , fn ( _ , glob ) => case glob of M.GCode code => M.GCode ( doCode code )| _ => glob ) 

 val prog = M.P { includes=includes , externs=externs , globals=gs , symbolTable=I.Manager.finish sm , entry=entry } 

 val _ = Debug.debug config print ( passname ^ ": with max-r1-size = " ^ Int.toString ( Control.getMaxR1Size config ) ^ ", found " ^ Int.toString ( ! ( # total stat ) ) ^ " loops, " ^ Int.toString ( ! ( # inverted stat ) ) ^ " inverted, " ^ Int.toString ( ! ( # aborted stat ) ) ^ " aborted.\n" )  in prog end  

 fun program ( mil , d ) = let 

 val config = PD.getConfig d 

 val mil = doGlobal ( config , mil ) 

 val () = PD.report ( d , passname )  in mil end  

 val description = { name=passname , description="Loop Invert" , inIr=BothMil.irHelpers , outIr=BothMil.irHelpers , mustBeAfter=[ ] , stats=stats } 

 val associates = { controls=[ Control.maxR1Size ] , debugs=[ Debug.debugPassD ] , features=[ ] , subPasses=[ ] } 

 val pass = Pass.mkOptPass ( description , associates , BothMil.mkMilPass program )  end 

