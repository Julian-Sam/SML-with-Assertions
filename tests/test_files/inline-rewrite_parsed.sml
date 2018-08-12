signature MIL_INLINE_REWRITER = sig val program : PassData.t * IMil.t * int option -> unit  end 

 datatype inlineOperation = InlineFunctionCopy | InlineFunction | CloneFunction | NoOp functor MilInlineRewriterF ( type policyInfo val analyze : PassData.t * IMil.t -> policyInfo type callId val callIdToCall : policyInfo * IMil.t * callId -> IMil.iInstr val associateCallToCallId : policyInfo * IMil.t * callId * IMil.iBlock * IMil.iBlock -> unit val rewriteOperation : callId -> inlineOperation val policy : policyInfo * PassData.t * IMil.t -> callId list val optimizer : ( policyInfo * PassData.t * IMil.t * IMil.iInstr list -> unit ) option  ) :> MIL_INLINE_REWRITER = struct 

 type policyInfo = policyInfo 

 val analyze = analyze 

 type callId = callId 

 val callIdToCall = callIdToCall 

 val associateCallToCallId = associateCallToCallId 

 val rewriteOperation = rewriteOperation 

 val policy = policy 

 val optimizer = optimizer structure PD = PassData structure M = Mil 

 fun fail ( f , m ) = Fail.fail ( "inline-rewrite.sml " , f , m )  

 fun optimizeIMil ( info : policyInfo , d : PassData.t , imil : IMil.t , ils : IMil.iInstr list ) : unit = case optimizer of NONE => ( )| SOME optimize => optimize ( info , d , imil , ils )  

 fun inlineCallSite ( info : policyInfo , d : PD.t , c : callId , imil : IMil.t , dup : bool ) : IMil.iInstr list = let 

 val callInstr : IMil.iInstr = callIdToCall ( info , imil , c ) 

 val milCall = case IMil.IInstr.toTransfer callInstr of SOME ( M.TInterProc { callee = M.IpCall { call , ... } , ... } ) => call| _ => fail ( "inlineCallSite" , "Invalid IMil call instruction." ) 

 val fname = case milCall of M.CCode { ptr , ... } => ptr| M.CDirectClosure { code , ... } => code| M.CClosure _ => fail ( "inlineCallSite" , "Cannot inline calls to CClosure." ) 

 fun mapBlk ( old : IMil.iBlock , new : IMil.iBlock ) : unit = associateCallToCallId ( info , imil , c , old , new )   in if dup then IMil.IFunc.inlineMap ( imil , fname , callInstr , SOME mapBlk , NONE ) else IMil.IFunc.inline ( imil , fname , callInstr ) end  

 fun cloneCallSite ( info : policyInfo , d : PD.t , c : callId , imil : IMil.t ) : IMil.iInstr list = fail ( "cloneCallSite" , "Function not implemented yet." )  

 fun rewriteCallSite ( info : policyInfo , d : PD.t , c : callId , imil : IMil.t ) : IMil.iInstr list = case rewriteOperation ( c ) of InlineFunction => inlineCallSite ( info , d , c , imil , false )| InlineFunctionCopy => inlineCallSite ( info , d , c , imil , true )| CloneFunction => cloneCallSite ( info , d , c , imil )| NoOp => nil  

 fun rewriteCallSites ( info : policyInfo , d : PD.t , callsToInline : callId list , imil : IMil.t ) : IMil.iInstr list = let 

 val ils = List.map ( callsToInline , fn c => rewriteCallSite ( info , d , c , imil ) )  in List.concat ( ils ) end  

 fun iterativeRewrite ( info : policyInfo , d : PD.t , imil : IMil.t , roundsO : int option ) : unit = let 

 val inline = fn () => case policy ( info , d , imil ) of [ ] => false| l => let 

 val il = rewriteCallSites ( info , d , l , imil ) 

 val () = optimizeIMil ( info , d , imil , il )  in true end 

 val inlineBounded = fn rounds => let 

 val rec loop = fn i => if i < rounds andalso inline ( ) then loop ( i + 1 ) else ( )  in loop 0 end 

 val rec inlineUnbounded = fn () => if inline ( ) then inlineUnbounded ( ) else ( )  in case roundsO of NONE => inlineUnbounded ( )| SOME rounds => inlineBounded rounds end  

 fun program ( d : PD.t , imil : IMil.t , roundsO : int option ) : unit = let 

 val info = analyze ( d , imil )  in iterativeRewrite ( info , d , imil , roundsO ) end   end 

