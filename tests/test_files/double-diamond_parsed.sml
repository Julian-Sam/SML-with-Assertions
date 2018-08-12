signature MIL_DBL_DIAMOND = sig val pass : ( BothMil.t , BothMil.t ) Pass.t  end structure MilDblDiamond :> MIL_DBL_DIAMOND = struct 

 val passname = "MilDblDiamond" 

 val stats = [ ( "SQLifted" , "PSetQueries lifted" ) ] structure U = Utils structure PD = PassData structure MU = MilUtils structure WS = IMil.WorkSet structure Instr = IMil.IInstr structure Block = IMil.IBlock structure Use = IMil.Use 

 type block = IMil.iBlock 

 fun fail ( f , m ) = Fail.fail ( "MilDblDiamond" , f , m )  

 val ( debugPassD , debugPass ) = Config.Debug.mk ( passname , "debug the Mil double diamond pass" ) 

 fun debug ( d : PD.t , m : string ) = if debugPass ( PD.getConfig d ) then print ( passname ^ ": " ^ m ^ "\n" ) else ( )  

 fun fixPredecessor ( imil : IMil.t , wl : WS.ws , blk : block , varIndex : int ) = let 

 val parameters = Block.getParameters ( imil , blk ) 

 val vi : Mil.variable = Vector.sub ( parameters , varIndex ) 

 val vi' : Mil.variable = IMil.Var.new ( imil , "mdd_vii_#" , MU.Bool.t ( IMil.T.getConfig imil ) , Mil.VkLocal ) 

 val opand : Mil.operand = Mil.SVariable ( vi ) 

 val newMilInstr = MU.Instruction.new ( vi' , Mil.RhsPSetQuery opand ) 

 val newInstr : IMil.iInstr = Block.append ( imil , blk , newMilInstr ) 

 val () = WS.addInstr ( wl , newInstr ) 

 val transferInstr : IMil.iInstr = Block.getTransfer ( imil , blk ) 

 val ( args , block ) = case Instr.toTransfer transferInstr of SOME ( Mil.TGoto ( Mil.T { block , arguments } ) ) => ( arguments , block )| _ => fail ( "fixPredecessor" , "Block optional transfer is not TGoto" ) 

 val newArguments = U.Vector.update ( args , varIndex , Mil.SVariable ( vi' ) ) 

 val newTarget = Mil.T { block=block , arguments=newArguments } 

 val newTransfer = Mil.TGoto newTarget 

 val () = WS.addInstr ( wl , transferInstr )  in Block.replaceTransfer ( imil , blk , newTransfer ) end  

 fun replacePSetQuery ( imil , wl , use , newVar ) = let 

 val iinstr = case Use.toIInstr use of SOME x => x| NONE => fail ( "replacePSetQuery" , "Invalid use" ) 

 val Mil.I { dests , ... } = case Instr.toInstruction iinstr of SOME x => x| NONE => fail ( "replacePSetQuery" , "Not a Mil instruction" ) 

 val newInstr = MU.Instruction.new' ( dests , Mil.RhsSimple ( Mil.SVariable newVar ) ) 

 val () = WS.addInstr ( wl , iinstr )  in IMil.IInstr.replaceInstruction ( imil , iinstr , newInstr ) end  

 fun processVariable ( imil : IMil.t , wl : WS.ws , label : Mil.label , varIndex : int , v : Mil.variable , predecessors : block list ) : Mil.variable = let 

 val newVar = IMil.Var.new ( imil , "mdd_#" , MU.Bool.t ( IMil.T.getConfig imil ) , Mil.VkLocal ) 

 val uses = IMil.Use.getUses ( imil , v ) 

 fun doOne ( use ) = replacePSetQuery ( imil , wl , use , newVar )  

 val () = Vector.foreach ( uses , doOne ) 

 fun doOne ( blk ) = fixPredecessor ( imil , wl , blk , varIndex )  

 val () = List.foreach ( predecessors , doOne )  in newVar end  

 fun useIsRhsPSetQuery ( imil : IMil.t , use : IMil.use ) : bool = case Use.toInstruction use of SOME ( Mil.I { rhs = Mil.RhsPSetQuery _ , ... } ) => true| _ => false  

 fun allUsesAreRhsPSetQuery ( d : PD.t , imil : IMil.t , v : Mil.variable ) : bool = let 

 val uses : IMil.use Vector.t = IMil.Use.getUses ( imil , v ) 

 val allRhsPSetQuery = Vector.forall ( uses , fn ( u ) => useIsRhsPSetQuery ( imil , u ) ) 

 val () = if ( allRhsPSetQuery ) then debug ( d , "  - [OK]: All uses are RhsPSetQuery." ) else debug ( d , "  - [FAILED]: Has an use with non RhsPSetQuery." )  in allRhsPSetQuery end  

 fun isMultipleInEdgeBlock ( imil : IMil.t , block : block ) : bool = let 

 val inEdges : ( block * block ) list = Block.inEdges ( imil , block ) ;  in length ( inEdges ) > 1 end  

 fun processLabel ( d : PD.t , imil : IMil.t , wl : WS.ws , labelInstr : IMil.iInstr ) : bool = let 

 val ( label , vars ) = case Instr.toLabel labelInstr of NONE => fail ( "processLabel" , "Invalid label" )| SOME x => x 

 val splitBlocks = ref NONE 

 val changed = ref false 

 val block = Instr.getIBlock ( imil , labelInstr ) 

 val () = debug ( d , "- Processing label \"" ^ Identifier.labelString ( label ) ^ "\"." ) 

 fun splitInputEdges () : ( block list ) option = Try.try ( fn () => case ! splitBlocks of SOME preds => preds| NONE => let 

 val ( preds , cut ) = Block.splitInEdges ( imil , block ) 

 val () = Try.require ( not cut ) 

 val () = splitBlocks := SOME preds 

 fun addLabelToWL ( blk ) = WS.addInstr ( wl , Block.getLabel ( imil , blk ) )  

 val () = List.foreach ( preds , addLabelToWL )  in preds end )  

 fun checkAndProcessVariable ( i : int , v : Mil.variable ) = Try.try ( fn () => let 

 val () = debug ( d , "  - check and process variable \"" ^ Identifier.variableString' v ^ "\"." )  in if allUsesAreRhsPSetQuery ( d , imil , v ) then let 

 val () = debug ( d , "     - all uses are RhsPSetQuery." ) 

 val predecessors = Try.<- ( splitInputEdges ( ) ) 

 val () = changed := true 

 val () = PD.click ( d , "SQLifted" )  in processVariable ( imil , wl , label , i , v , predecessors ) end else v end )  

 val _ = Try.try ( fn () => let 

 val () = Try.require ( isMultipleInEdgeBlock ( imil , block ) ) 

 val () = debug ( d , "  - Label has multiple input edges." ) ; 

 val newVars = Vector.mapi ( vars , ( Try.<- o checkAndProcessVariable ) ) 

 val () = debug ( d , "  - Replacing label." ) ; 

 val () = WS.addInstr ( wl , labelInstr )  in Instr.replaceMil ( imil , labelInstr , IMil.MLabel ( label , newVars ) ) end )  in ! changed end  

 fun processFunc ( d : PD.t , imil : IMil.t , f : IMil.iFunc ) : unit = let 

 val wl = WS.new ( ) 

 val labels : IMil.iInstr list = IMil.Enumerate.IFunc.labels ( imil , f ) 

 fun doLabel ( l , c ) = c orelse processLabel ( d , imil , wl , l )  

 val changed = List.fold ( labels , false , doLabel ) 

 val () = MilCfgSimplify.function ( d , imil , f ) 

 val () = MilSimplify.simplify ( d , imil , wl )  in if changed then processFunc ( d , imil , f ) else ( ) end  

 fun processFuncs ( d : PD.t , imil : IMil.t ) : unit = let 

 val funcs = IMil.Enumerate.T.funcs imil  in List.foreach ( funcs , fn f => processFunc ( d , imil , f ) ) end  

 fun program ( imil : IMil.t , pd : PD.t ) : unit = let 

 val () = debug ( pd , " - Starting the double diamond reduction..." ) 

 val () = processFuncs ( pd , imil ) 

 val () = PD.report ( pd , passname )  in ( ) end  

 val description = { name=passname , description="Lift set queries above phis" , inIr=BothMil.irHelpers , outIr=BothMil.irHelpers , mustBeAfter=[ ] , stats=stats } 

 val associates = { controls=[ ] , debugs=[ debugPassD ] , features=[ ] , subPasses=[ ] } 

 val pass = Pass.mkOptPass ( description , associates , BothMil.mkIMilPass program )  end 

