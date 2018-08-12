signature MIL_CHECK = sig val program' : Config.t * Mil.t -> bool val program : Config.t * Mil.t -> unit  end ; structure MilCheck :> MIL_CHECK = struct structure SS = StringSet structure IA = IntArb structure I = Identifier structure IM = I.Manager structure VS = I.VariableSet structure VD = I.VariableDict structure ND = I.NameDict structure LS = I.LabelSet structure LD = I.LabelDict structure M = Mil structure MP = Mil.Prims structure MU = MilUtils structure TS = MU.TraceabilitySize structure MUT = MU.Typ structure MT = MilType structure MUP = MilUtils.Prims structure PU = MUP.Utils 

 datatype state = S of { failed : bool ref , varsBound : VS.t ref , labsBound : LS.t ref , incFiles : SS.t ref } 

 fun stateMk () = S { failed=ref false , varsBound=ref VS.empty , labsBound=ref LS.empty , incFiles=ref SS.empty }  

 fun setFailed ( S { failed , ... } ) = failed := true  

 fun isVarBound ( S { varsBound , ... } , v ) = VS.member ( ! varsBound , v )  

 fun addVarBound ( S { varsBound , ... } , v ) = varsBound := VS.insert ( ! varsBound , v )  

 fun isLabBound ( S { labsBound , ... } , l ) = LS.member ( ! labsBound , l )  

 fun addLabBound ( S { labsBound , ... } , l ) = labsBound := LS.insert ( ! labsBound , l )  

 fun isIncFile ( S { incFiles , ... } , f ) = SS.member ( ! incFiles , f )  

 fun addIncFile ( S { incFiles , ... } , f ) = incFiles := SS.insert ( ! incFiles , f )  

 fun stateFinish ( S { failed , ... } ) = ! failed  

 datatype env = E of { c : Config.t , st : M.symbolTable , ord : M.name , varsAvail : VS.t , labsAvail : M.typ Vector.t LD.t , rtyps : Mil.typ Vector.t } 

 fun envMk ( c , st , ord ) = E { c=c , st=st , ord=ord , varsAvail=VS.empty , labsAvail=LD.empty , rtyps=Vector.new0 ( ) }  

 fun getConfig ( E { c , ... } ) = c  

 fun getSt ( E { st , ... } ) = st  

 fun getSi e = I.SymbolInfo.SiTable ( getSt e )  

 fun getOrd ( E { ord , ... } ) = ord  

 fun getTyp ( e , v ) = MU.SymbolTable.variableTyp ( getSt e , v )  

 fun varKind ( e , v ) = MU.SymbolTable.variableKind ( getSt e , v )  

 fun isVarAvailable ( E { varsAvail , ... } , v ) = VS.member ( varsAvail , v )  

 fun addVarAvailable ( E { c , st , ord , varsAvail , labsAvail , rtyps } , v ) = E { c=c , st=st , ord=ord , varsAvail=VS.insert ( varsAvail , v ) , labsAvail=labsAvail , rtyps=rtyps }  

 fun isLabAvailable ( E { labsAvail , ... } , l ) = LD.lookup ( labsAvail , l )  

 fun addLabAvailable ( E { c , st , ord , varsAvail , labsAvail , rtyps } , l , ts ) = E { c=c , st=st , ord=ord , varsAvail=varsAvail , labsAvail=LD.insert ( labsAvail , l , ts ) , rtyps=rtyps }  

 fun getRTyps ( E { rtyps , ... } ) = rtyps  

 fun setRTyps ( E { c , st , ord , varsAvail , labsAvail , rtyps } , rts ) = E { c=c , st=st , ord=ord , varsAvail=varsAvail , labsAvail=labsAvail , rtyps=rts }  

 fun reportError ( s , msg ) = let 

 val () = setFailed s 

 val () = print ( "MilCheck: " ^ msg ^ "\n" )  in ( ) end  

 fun assert ( s , b , msg ) = if not b then reportError ( s , msg ( ) ) else ( )  

 fun variable ( s , e , v ) = let 

 val t = if I.variableExists ( getSt e , v ) then getTyp ( e , v ) else let 

 val () = reportError ( s , "variable " ^ I.variableString' v ^ " not in symbol table" )  in M.TNone end  in t end  

 fun variables ( s , e , vs ) = Vector.map ( vs , fn v => variable ( s , e , v ) )  

 fun name ( s , e , n ) = let 

 val () = assert ( s , I.nameExists ( getSt e , n ) , fn () => "name " ^ I.nameString' n ^ " not in symbol table" )  in ( ) end  

 fun label ( s , e , l ) = let 

 val () = assert ( s , I.labelExists ( getSt e , l ) , fn () => "label " ^ I.labelString l ^ " not in symbol table" )  in ( ) end  

 fun callConvU ( s , e , f , m , cc ) = case cc of M.CcCode => ( )| M.CcUnmanaged _ => ( )| M.CcClosure { cls , fvs } => let 

 val () = f ( s , e , m , cls ) 

 val () = Vector.foreach ( fvs , fn x => f ( s , e , m , x ) )  in ( ) end| M.CcThunk { thunk , fvs } => let 

 val () = f ( s , e , m , thunk ) 

 val () = Vector.foreach ( fvs , fn x => f ( s , e , m , x ) )  in ( ) end  

 fun callConvE ( s , e , f , cc ) = case cc of M.CcCode => e| M.CcUnmanaged _ => e| M.CcClosure { cls , fvs } => let 

 val e = f ( s , e , cls ) 

 val e = Vector.fold ( fvs , e , fn ( x , e ) => f ( s , e , x ) )  in e end| M.CcThunk { thunk , fvs } => let 

 val e = f ( s , e , thunk ) 

 val e = Vector.fold ( fvs , e , fn ( x , e ) => f ( s , e , x ) )  in e end  

 fun optIntegerRep ( s , e , msg , i ) = assert ( s , MU.Integer.Opt.integerFits i , fn () => msg ( ) ^ ": integer constant out of opt rep range" )  

 fun optRatRep ( s , e , msg , r ) = assert ( s , MU.Rational.Opt.integerFits r , fn () => msg ( ) ^ ": rat constant out of opt rep range" )  

 fun validRefConstant ( s , e , msg , i ) = assert ( s , MU.HeapModel.validRefConstant ( getConfig e , i ) , fn () => msg ( ) ^ ": ref value not valid as ref constant" )  

 fun constant ( s , e , msg , c ) = case c of M.CRat r => let 

 val () = optRatRep ( s , e , msg , r )  in MUP.NumericTyp.tRat end| M.CInteger i => let 

 val () = optIntegerRep ( s , e , msg , i )  in MUP.NumericTyp.tIntegerArbitrary end| M.CName n => let 

 val () = name ( s , e , n )  in M.TName end| M.CIntegral i => MUP.NumericTyp.tIntegerFixed ( IntArb.typOf i )| M.CBoolean b => M.TBoolean| M.CFloat f => MUP.NumericTyp.tFloat| M.CDouble d => MUP.NumericTyp.tDouble| M.CViMask { descriptor , elts } => let 

 val () = assert ( s , PU.VectorDescriptor.elementCount descriptor = Vector.length elts , fn () => msg ( ) ^ ": bad constant mask" )  in M.TViMask descriptor end| M.CRef i => let 

 val () = validRefConstant ( s , e , msg , i )  in M.TRef end| M.COptionSetEmpty => M.TPType { kind=M.TkE , over=M.TNone }| M.CTypePH => M.TPType { kind=M.TkI , over=M.TNone }  

 fun sumTag ( s , e , msg , c ) = let 

 val t = constant ( s , e , msg , c ) 

 val () = case c of M.CBoolean _ => ( )| M.CRat _ => reportError ( s , msg ( ) ^ ": rational used as sum tag" )| M.CInteger _ => reportError ( s , msg ( ) ^ ": integer used as sum tag" )| M.CName _ => ( )| M.CIntegral _ => ( )| M.CFloat _ => reportError ( s , msg ( ) ^ ": float used as sum tag" )| M.CDouble _ => reportError ( s , msg ( ) ^ ": double used as sum tag" )| M.CViMask _ => reportError ( s , msg ( ) ^ ": mask used as sum tag" )| M.CRef _ => reportError ( s , msg ( ) ^ ": ref used as sum tag" )| M.COptionSetEmpty => reportError ( s , msg ( ) ^ ": option set used as sum tag" )| M.CTypePH => reportError ( s , msg ( ) ^ ": type used as sum tag" )  in t end  

 fun typ ( s , e , m , t ) = case t of M.TAny => ( )| M.TAnyS vs => ( )| M.TNonRefPtr => ( )| M.TRef => ( )| M.TBits vs => ( )| M.TNone => ( )| M.TNumeric _ => ( )| M.TBoolean => ( )| M.TName => ( )| M.TViVector et => typ ( s , e , m , # elementTyp et )| M.TViMask et => ( )| M.TCode { cc , args , ress } => let 

 val () = callConvU ( s , e , typ , m , cc ) 

 val () = typs ( s , e , m , args ) 

 val () = typs ( s , e , m , ress )  in ( ) end| M.TTuple { fixed , array } => let 

 val () = Vector.foreach ( fixed , fn ( t , _ , _ ) => typ ( s , e , m , t ) ) 

 val () = typ ( s , e , m , # 1 array )  in ( ) end| M.TCString => ( )| M.TIdx => ( )| M.TContinuation ts => typs ( s , e , m , ts )| M.TThunk t => typ ( s , e , m , t )| M.TPAny => ( )| M.TClosure { args , ress } => let 

 val () = typs ( s , e , m , args ) 

 val () = typs ( s , e , m , ress )  in ( ) end| M.TSum { tag , arms } => let 

 fun checkOne ( k , v ) = let 

 val _ = sumTag ( s , e , m , k ) 

 val () = typs ( s , e , m , v )  in ( ) end  

 val () = typ ( s , e , m , tag ) 

 val () = if Utils.SortedVectorMap.isSorted MU.Constant.compare arms then ( ) else reportError ( s , m ( ) ^ ": sum arms not sorted" ) 

 val () = Vector.foreach ( arms , checkOne )  in ( ) end| M.TPType { kind , over } => typ ( s , e , m , over )| M.TPRef t => typ ( s , e , m , t ) and typs ( s , e , m , ts ) = Vector.foreach ( ts , fn t => typ ( s , e , m , t ) )  

 fun knownTraceSize ( s , e , t , msg ) = case MUT.traceabilitySize ( getConfig e , t ) of TS.TsAny => reportError ( s , msg ( ) ^ ": bad traceability and size" )| TS.TsAnyS _ => reportError ( s , msg ( ) ^ ": bad traceability" )| TS.TsBits _ => ( )| TS.TsFloat => ( )| TS.TsDouble => ( )| TS.TsRef => ( )| TS.TsNone => ( )| TS.TsMask _ => ( )  

 fun checkConsistentTyp ( s , e , msg , t1 , t2 ) = let 

 val c = getConfig e 

 val ts1 = MUT.traceabilitySize ( c , t1 ) 

 val ts2 = MUT.traceabilitySize ( c , t2 ) 

 fun err () = reportError ( s , msg ( ) )   in ( ) end  

 fun checkConsistentTyps ( s , e , msg , ts1 , ts2 ) = let 

 fun msg' i () = msg ( ) ^ ": " ^ Int.toString i ^ ": type mismatch"  

 fun checkOne ( i , t1 , t2 ) = checkConsistentTyp ( s , e , msg' i , t1 , t2 )   in if Vector.length ts1 = Vector.length ts2 then Vector.foreachi2 ( ts1 , ts2 , checkOne ) else reportError ( s , msg ( ) ^ ": number mismatch" ) end  

 fun bindVar ( s , e , v , k ) = let 

 val t = variable ( s , e , v ) 

 val () = let 

 val k' = varKind ( e , v ) 

 val kindCheck = k = k' 

 val () = assert ( s , kindCheck , fn () => "variable " ^ I.variableString' v ^ ": kind property inconsistent with binder" )  in ( ) end handle _ => ( ) 

 val () = assert ( s , not ( isVarBound ( s , v ) ) , fn () => "variable " ^ I.variableString' v ^ " bound twice" ) 

 val () = addVarBound ( s , v ) 

 val env = addVarAvailable ( e , v ) 

 fun msg () = "variable " ^ I.variableString' v  

 val () = knownTraceSize ( s , e , t , msg )  in env end  

 fun bindVars ( s , e , vs , k ) = Vector.fold ( vs , e , fn ( v , e ) => bindVar ( s , e , v , k ) )  

 fun bindVarsL ( s , e , vs , k ) = List.fold ( vs , e , fn ( v , e ) => bindVar ( s , e , v , k ) )  

 fun bindVarTo ( s , e , v , t ) = let 

 fun msg () = I.variableString' v ^ " binding inconsistent with type"   in checkConsistentTyp ( s , e , msg , variable ( s , e , v ) , t ) end  

 fun bindVarsTo ( s , e , msg , vs , ts ) = let 

 val () = assert ( s , Vector.length vs = Vector.length ts , fn () => msg ( ) ^ ": arity mismatch in binding" ) 

 val () = if Vector.length vs = Vector.length ts then Vector.foreach2 ( vs , ts , fn ( v , t ) => bindVarTo ( s , e , v , t ) ) else ( )  in ( ) end  

 fun variableUse ( s , e , msg , v ) = let 

 val () = assert ( s , isVarAvailable ( e , v ) , fn () => msg ( ) ^ ": variable " ^ I.variableString' v ^ " not in scope" ) 

 val t = variable ( s , e , v )  in t end  

 fun bindLabel ( s , e , l , ts ) = let 

 val () = assert ( s , not ( isLabBound ( s , l ) ) , fn () => "label " ^ I.labelString l ^ " bound twice" ) 

 val () = addLabBound ( s , l ) 

 val env = addLabAvailable ( e , l , ts )  in env end  

 fun labelUseNoArgs ( s , e , msg , l ) = let 

 val () = label ( s , e , l ) 

 fun notInScope () = reportError ( s , msg ( ) ^ ": label " ^ I.labelString l ^ " not in scope" )  

 val () = case isLabAvailable ( e , l ) of NONE => notInScope ( )| SOME _ => ( )  in ( ) end  

 fun labelUse ( s , e , msg , l , ts ) = let 

 val () = label ( s , e , l ) 

 fun msg' () = msg ( ) ^ ": arguments to " ^ I.labelString l  

 fun notInScope () = reportError ( s , msg ( ) ^ ": label " ^ I.labelString l ^ " not in scope" )  

 val () = case isLabAvailable ( e , l ) of NONE => notInScope ( )| SOME ts' => checkConsistentTyps ( s , e , msg' , ts' , ts )  in ( ) end  

 fun simple ( s , e , msg , simp ) = case simp of M.SConstant c => constant ( s , e , msg , c )| M.SVariable v => variableUse ( s , e , msg , v )  

 fun simples ( s , e , msg , ss ) = let 

 fun msg' i () = msg ( ) ^ ": " ^ Int.toString i  

 fun doOne ( i , simp ) = simple ( s , e , msg' i , simp )   in Vector.mapi ( ss , doOne ) end  

 fun operand ( s , e , msg , opnd ) = simple ( s , e , msg , opnd )  

 fun operands ( s , e , msg , os ) = let 

 fun msg' i () = msg ( ) ^ ": " ^ Int.toString i  

 fun doOne ( i , opnd ) = operand ( s , e , msg' i , opnd )   in Vector.mapi ( os , doOne ) end  

 fun consistentFieldKind ( s , e , msg , fd , t ) = let 

 val efd = MUT.traceabilitySize ( getConfig e , t ) 

 fun err () = reportError ( s , msg ( ) ^ ": field kind mismatch, expect " ^ MU.FieldKind.toString fd ^ " but got " ^ MU.TraceabilitySize.toString ( getConfig e , efd ) ^ "\n" )   in case ( fd , efd ) of ( M.FkRef , TS.TsRef ) => ( )| ( M.FkBits fs , TS.TsBits vs ) => if MU.FieldSize.toValueSize fs = vs then ( ) else err ( )| ( M.FkFloat , TS.TsFloat ) => ( )| ( M.FkDouble , TS.TsDouble ) => ( )| ( _ , TS.TsNone ) => ( )| _ => err ( ) end  local  exception Done  in 

 fun consistentTupDesc ( s , e , msg , td , ts , ns , ao ) = let 

 val M.TD { fixed , array } = td 

 fun msgi i () = msg ( ) ^ ": " ^ Int.toString i  

 fun checkOne ( i , t ) = let 

 val M.FD { kind , ... } = if i < Vector.length fixed then Vector.sub ( fixed , i ) else case array of NONE => raise Done| SOME fd => fd 

 val n = i < Vector.length ns andalso Vector.sub ( ns , i ) 

 val () = if n then case kind of M.FkRef => ( )| _ => consistentFieldKind ( s , e , msgi i , kind , t ) else consistentFieldKind ( s , e , msgi i , kind , t )  in ( ) end  

 val () = ( Vector.foreachi ( ts , checkOne ) handle Done => ( ) ) 

 val () = case ao of NONE => if Option.isSome array then reportError ( s , msg ( ) ^ ": array portion mismatch" ) else ( )| SOME t1 => case array of NONE => ( )| SOME ( M.FD { kind = t2 , ... } ) => consistentFieldKind ( s , e , fn () => msg ( ) ^ ": array portion" , t2 , t1 )  in ( ) end  

 fun consistentMdDesc ( s , e , msg , vtd , ts ) = let 

 val M.MDD { pinned , fixed , array } = vtd 

 fun tooManyFields () = reportError ( s , msg ( ) ^ ": not enough fields in vtable descriptor" )  

 fun msgi i () = msg ( ) ^ ": " ^ Int.toString i  

 fun checkOne ( i , t ) = let 

 val M.FD { kind , ... } = if i < Vector.length fixed then Vector.sub ( fixed , i ) else case array of NONE => let 

 val () = tooManyFields ( )  in raise Done end| SOME ( _ , fd ) => fd 

 val () = consistentFieldKind ( s , e , msgi i , kind , t )  in ( ) end  

 val () = ( Vector.foreachi ( ts , checkOne ) handle Done => ( ) )  in ( ) end   end 

 fun tupleMake ( s , e , msg , mdd , inits ) = let 

 fun msg' () = msg ( ) ^ ": init"  

 val ts = operands ( s , e , msg' , inits ) 

 val () = consistentMdDesc ( s , e , msg , mdd , ts ) 

 val M.MDD { fixed , array , ... } = mdd 

 val () = case array of NONE => ( )| SOME ( lenIdx , _ ) => assert ( s , 0 <= lenIdx andalso lenIdx < Vector.length ts , fn () => msg ( ) ^ ": length field not inited" ) 

 val get = fn ( i , t ) => let 

 val ( var , a ) = case MU.MetaDataDescriptor.getField ( mdd , i ) of SOME fd => let 

 val var = MU.FieldDescriptor.var fd 

 val a = MU.FieldDescriptor.alignment fd  in ( var , a ) end| NONE => let 

 val () = reportError ( s , msg ( ) ^ ": not enough fields" )  in ( M.FvReadWrite , M.Vs8 ) end  in ( t , a , var ) end 

 val tvs = Vector.mapi ( ts , get ) 

 val t = MU.Typ.fixedArray tvs  in t end  

 fun prim ( s , e , msg , prim , typs , args ) = let 

 fun msg' () = msg ( ) ^ ": arg"  

 val ts = operands ( s , e , msg' , args ) 

 val config = getConfig e 

 val si = getSi e 

 val ( ats , rts ) = MT.PrimsTyper.t ( config , si , prim , typs ) 

 val t = let 

 fun msg' () = msg ( ) ^ ": argument number mismatch"  

 val () = assert ( s , Vector.length args = Vector.length ats , msg' ) 

 val c = getConfig e 

 fun msgi i = msg ( ) ^ ": arg " ^ Int.toString i  

 fun checkOne ( i , t1 ) = if MT.Type.subtype ( config , Vector.sub ( ts , i ) , t1 ) then ( ) else reportError ( s , msgi i ^ ": type mismatch" )   in rts end  in t end  

 fun tupleField ( s , e , msg , M.TF { tupDesc , tup , field } ) = let 

 fun msg' () = msg ( ) ^ ": tuple"  

 val t = variableUse ( s , e , msg' , tup ) 

 val ft = case field of M.FiFixed idx => ( case t of M.TTuple { fixed , array , ... } => # 1 ( if idx < Vector.length fixed then Vector.sub ( fixed , idx ) else array )| _ => M.TNone )| M.FiVariable opnd => let 

 fun msg' () = msg ( ) ^ ": index"  

 val _ = operand ( s , e , msg' , opnd )  in case t of M.TTuple { array , ... } => # 1 array| _ => M.TNone end| M.FiVectorFixed { descriptor , mask , index } => let 

 fun msg' () = msg ( ) ^ ": index"  

 val _ = Option.map ( mask , fn opnd => operand ( s , e , msg' , opnd ) )  in M.TNone end| M.FiVectorVariable { descriptor , base , mask , index , kind } => let 

 fun msg' () = msg ( ) ^ ": index"  

 val _ = Option.map ( mask , fn opnd => operand ( s , e , msg' , opnd ) ) 

 val _ = operand ( s , e , msg' , index )  in M.TNone end  in ft end  

 fun inRange ( s , e , msg , v , i ) = if i < Vector.length v then ( ) else reportError ( s , msg ( ) ^ ": index not in range" )  

 fun sameLength ( s , e , msg , v1 , v2 ) = if Vector.length v1 = Vector.length v2 then ( ) else reportError ( s , msg ( ) ^ ": mismatched lengths" )  

 fun rhs ( s , e , msg , r ) = let 

 val none = Vector.new0 ( ) 

 val some = Vector.new1 

 val ts = ( case r of M.RhsSimple simp => some ( simple ( s , e , msg , simp ) )| M.RhsPrim { prim = p , createThunks , typs , args } => prim ( s , e , msg , p , typs , args )| M.RhsTuple { mdDesc , inits } => some ( tupleMake ( s , e , msg , mdDesc , inits ) )| M.RhsTupleSub tf => some ( tupleField ( s , e , msg , tf ) )| M.RhsTupleSet { tupField , ofVal } => let 

 val ft = tupleField ( s , e , msg , tupField ) 

 val nvt = operand ( s , e , msg , ofVal ) 

 fun msg' () = msg ( ) ^ ": field/value type mismatch"  

 val () = checkConsistentTyp ( s , e , msg' , ft , nvt )  in none end| M.RhsTupleCAS { tupField , cmpVal , newVal } => let 

 val ft = tupleField ( s , e , msg , tupField ) 

 val cvt = operand ( s , e , msg , cmpVal ) 

 fun msg' () = msg ( ) ^ ": field / compare value type mismatch"  

 val () = checkConsistentTyp ( s , e , msg' , ft , cvt ) 

 val nvt = operand ( s , e , msg , newVal ) 

 fun msg' () = msg ( ) ^ ": field / new value type mismatch"  

 val () = checkConsistentTyp ( s , e , msg' , ft , nvt )  in some ft end| M.RhsTupleWait { tupField as M.TF { field , ... } , pred } => let 

 val ft = tupleField ( s , e , msg , tupField ) 

 val () = if MUT.isRef ft then ( ) else reportError ( s , msg ( ) ^ ": field must be ref" ) 

 val () = case field of M.FiFixed _ => ( )| M.FiVariable _ => ( )| M.FiVectorFixed _ => reportError ( s , msg ( ) ^ ": field must be scalar" )| M.FiVectorVariable _ => reportError ( s , msg ( ) ^ ": field must be scalar" )  in none end| M.RhsTupleInited { mdDesc , tup } => let 

 fun msg' () = msg ( ) ^ ": tuple"  

 val _ = variableUse ( s , e , msg' , tup )  in none end| M.RhsIdxGet { idx , ofVal } => let 

 fun msg1 () = msg ( ) ^ ": index"  

 val _ = variableUse ( s , e , msg1 , idx ) 

 fun msg2 () = msg ( ) ^ ": of val"  

 val _ = operand ( s , e , msg2 , ofVal )  in some ( MU.Uintp.t ( getConfig e ) ) end| M.RhsCont l => let 

 val t = case isLabAvailable ( e , l ) of NONE => M.TNone| SOME ts => M.TContinuation ts  in some t end| M.RhsThunkMk { typ , fvs } => let  in some ( M.TThunk M.TNone ) end| M.RhsThunkInit { typ , thunk , fx , code , fvs } => let 

 fun msg1 () = msg ( ) ^ ": thunk"  

 val () = case thunk of NONE => ( )| SOME v => ignore ( variableUse ( s , e , msg1 , v ) ) 

 fun msg2 () = msg ( ) ^ ": code"  

 val () = case code of NONE => ( )| SOME v => ignore ( variableUse ( s , e , msg2 , v ) ) 

 fun doOne ( i , ( fk , opnd ) ) = let 

 fun msg' () = msg ( ) ^ ": free variable " ^ Int.toString i  

 val _ = operand ( s , e , msg' , opnd )  in ( ) end  

 val () = Vector.foreachi ( fvs , doOne ) 

 val ts = case thunk of SOME _ => none| NONE => some ( M.TThunk M.TNone )  in ts end| M.RhsThunkGetFv { typ , fvs , thunk , idx } => let 

 fun msg' () = msg ( ) ^ ": thunk get fv"  

 val _ = variableUse ( s , e , msg' , thunk ) 

 val () = inRange ( s , e , msg' , fvs , idx )  in some M.TNone end| M.RhsThunkValue { typ , thunk , ofVal } => let 

 fun msg1 () = msg ( ) ^ ": thunk value"  

 val () = case thunk of NONE => ( )| SOME v => ignore ( variableUse ( s , e , msg1 , v ) ) 

 fun msg2 () = msg ( ) ^ ": val"  

 val _ = operand ( s , e , msg2 , ofVal ) 

 val ts = case thunk of SOME _ => none| NONE => some M.TNone  in ts end| M.RhsThunkGetValue { typ , thunk } => let 

 fun msg' () = msg ( ) ^ ": thunk get value"  

 val _ = variableUse ( s , e , msg' , thunk )  in some M.TNone end| M.RhsThunkSpawn { typ , thunk , fx } => let 

 fun msg' () = msg ( ) ^ ": thunk spawn"  

 val _ = variableUse ( s , e , msg' , thunk )  in none end| M.RhsClosureMk { fvs } => let  in some M.TNone end| M.RhsClosureInit { cls , code , fvs } => let 

 fun msg1 () = msg ( ) ^ ": closure"  

 val () = case cls of NONE => ( )| SOME v => ignore ( variableUse ( s , e , msg1 , v ) ) 

 fun msg2 () = msg ( ) ^ ": code"  

 val () = case code of NONE => ( )| SOME v => ignore ( variableUse ( s , e , msg2 , v ) ) 

 fun doOne ( i , ( fk , opnd ) ) = let 

 fun msg' () = msg ( ) ^ ": free variable " ^ Int.toString i  

 val t = operand ( s , e , msg' , opnd ) 

 val () = consistentFieldKind ( s , e , msg' , fk , t )  in ( ) end  

 val () = Vector.foreachi ( fvs , doOne ) 

 val ts = case cls of SOME _ => none| NONE => some M.TNone  in ts end| M.RhsClosureGetFv { fvs , cls , idx } => let 

 fun msg' () = msg ( ) ^ ": closure get fv"  

 val _ = variableUse ( s , e , msg' , cls ) 

 val () = inRange ( s , e , msg' , fvs , idx )  in some M.TNone end| M.RhsPSetNew opnd => let 

 fun msg' () = msg ( ) ^ ": of val"  

 val _ = operand ( s , e , msg' , opnd )  in some M.TNone end| M.RhsPSetGet v => let 

 fun msg' () = msg ( ) ^ ": set"  

 val _ = variableUse ( s , e , msg' , v )  in some M.TNone end| M.RhsPSetCond { bool , ofVal } => let 

 fun msg1 () = msg ( ) ^ ": boolean"  

 val _ = operand ( s , e , msg1 , bool ) 

 fun msg2 () = msg ( ) ^ ": of val"  

 val _ = operand ( s , e , msg2 , ofVal )  in some M.TNone end| M.RhsPSetQuery oper => let 

 fun msg' () = msg ( ) ^ ": set"  

 val _ = operand ( s , e , msg' , oper )  in some M.TNone end| M.RhsEnum { tag , typ } => let 

 val _ = operand ( s , e , msg , tag )  in some M.TNone end| M.RhsSum { tag , typs , ofVals } => let 

 val _ = sumTag ( s , e , msg , tag ) 

 fun msg2 () = msg ( ) ^ ": of vals"  

 val ts = operands ( s , e , msg2 , ofVals ) 

 val () = sameLength ( s , e , msg2 , typs , ofVals ) 

 val () = Vector.foreach2 ( typs , ts , fn ( fk , t ) => consistentFieldKind ( s , e , msg2 , fk , t ) )  in some M.TNone end| M.RhsSumProj { typs , sum , tag , idx } => let 

 fun msg1 () = msg ( ) ^ ": sum proj"  

 val _ = variableUse ( s , e , msg1 , sum ) 

 val _ = sumTag ( s , e , msg1 , tag ) 

 val () = inRange ( s , e , msg1 , typs , idx )  in some M.TNone end| M.RhsSumGetTag { typ , sum } => let 

 fun msg1 () = msg ( ) ^ ": sum get tag"  

 val _ = variableUse ( s , e , msg1 , sum )  in some M.TNone end )  in ts end  

 fun instruction ( s , e , msg , ( M.I { dests , n , rhs = r } ) ) = let 

 val ts = rhs ( s , e , msg , r ) 

 val () = bindVarsTo ( s , e , msg , dests , ts ) 

 val e = bindVars ( s , e , dests , M.VkLocal )  in e end  

 fun target ( s , e , msg , M.T { block , arguments } ) = let 

 fun msg' () = msg ( ) ^ ": args"  

 val ts = operands ( s , e , msg' , arguments ) 

 fun msg' () = msg ( ) ^ ": label"  

 val () = labelUse ( s , e , msg' , block , ts )  in ( ) end  

 fun switch ( s , e , msg , { select , on , cases , default } ) = let 

 fun msg' () = msg ( ) ^ ": on"  

 val ot = operand ( s , e , msg' , on ) 

 fun checkOne ( j , ( x , t ) ) = let 

 fun msg' () = msg ( ) ^ ": arm " ^ Int.toString j  

 fun msg'' () = msg' ( ) ^ ": val"  

 val ct = constant ( s , e , msg'' , x ) 

 fun msg'' () = msg' ( ) ^ ": target"  

 val () = target ( s , e , msg'' , t ) 

 fun msg'' () = msg' ( ) ^ ": val/on"  

 val () = checkConsistentTyp ( s , e , msg'' , ot , ct )  in ( ) end  

 val () = Vector.foreachi ( cases , checkOne ) 

 fun msg' () = msg ( ) ^ ": default target"  

 val () = Option.foreach ( default , fn t => target ( s , e , msg' , t ) )  in ( ) end  

 fun getCodeType ( s , e , msg , ct ) = let 

 fun err () = reportError ( s , msg ( ) ^ ": bad code type" )   in case ct of M.TAny => ( NONE , NONE )| M.TAnyS vs => let 

 val () = if vs = MU.ValueSize.ptrSize ( getConfig e ) then ( ) else err ( )  in ( NONE , NONE ) end| M.TNonRefPtr => ( NONE , NONE )| M.TBits vs => let 

 val () = if vs = MU.ValueSize.ptrSize ( getConfig e ) then ( ) else err ( )  in ( NONE , NONE ) end| M.TNone => ( NONE , NONE )| M.TCode { args , ress , ... } => ( SOME args , SOME ress )| _ => let 

 val () = err ( )  in ( NONE , NONE ) end end  

 fun getClosureType ( s , e , msg , ct ) = let 

 fun err () = reportError ( s , msg ( ) ^ ": bad closure type" )   in case ct of M.TAny => ( NONE , NONE )| M.TAnyS vs => let 

 val () = if vs = MU.ValueSize.ptrSize ( getConfig e ) then ( ) else err ( )  in ( NONE , NONE ) end| M.TRef => ( NONE , NONE )| M.TPAny => ( NONE , NONE )| M.TNone => ( NONE , NONE )| M.TClosure { args , ress , ... } => ( SOME args , SOME ress )| _ => let 

 val () = err ( )  in ( NONE , NONE ) end end  

 fun codes ( s , e , msg , { possible , exhaustive } : M.codes , ats , rts ) = let 

 fun checkOne v = let 

 val t = variableUse ( s , e , msg , v )  in ( ) end  

 val () = VS.foreach ( possible , checkOne )  in ( ) end  

 fun call ( s , e , msg , c ) = case c of M.CCode { ptr , code } => let 

 val ct = variableUse ( s , e , msg , ptr ) 

 val ( ats , rts ) = getCodeType ( s , e , msg , ct ) 

 val () = codes ( s , e , msg , code , ats , rts )  in ( ats , rts ) end| M.CClosure { cls , code } => let 

 val clst = variableUse ( s , e , msg , cls ) 

 val ( ats , rts ) = getClosureType ( s , e , msg , clst ) 

 val () = codes ( s , e , msg , code , ats , rts )  in ( ats , rts ) end| M.CDirectClosure { cls , code } => let 

 val clst = variableUse ( s , e , msg , cls ) 

 val _ = getClosureType ( s , e , msg , clst ) 

 val codet = variableUse ( s , e , msg , code ) 

 val ( ats , rts ) = getCodeType ( s , e , msg , codet )  in ( ats , rts ) end  

 fun getThunkType ( s , e , msg , tt ) = let 

 fun err () = reportError ( s , msg ( ) ^ ": bad thunk type" )   in case tt of M.TAny => M.TNone| M.TAnyS vs => let 

 val () = if vs = MU.ValueSize.ptrSize ( getConfig e ) then ( ) else err ( )  in M.TNone end| M.TRef => M.TNone| M.TNone => M.TNone| M.TThunk t => t| _ => let 

 val () = err ( )  in M.TNone end end  

 fun eval ( s , e , msg , evl ) = case evl of M.EThunk { thunk , value , code } => let 

 val tt = variableUse ( s , e , msg , thunk ) 

 val t = getThunkType ( s , e , msg , tt ) 

 val ats = SOME ( Vector.new0 ( ) ) 

 val rts = SOME ( Vector.new1 t ) 

 val () = codes ( s , e , msg , code , ats , rts )  in t end| M.EDirectThunk { thunk , value , code } => let 

 val tt = variableUse ( s , e , msg , thunk ) 

 val t = getThunkType ( s , e , msg , tt ) 

 val codet = variableUse ( s , e , msg , code ) 

 val _ = getCodeType ( s , e , msg , codet )  in t end  

 fun interProc ( s , e , msg , ip ) = case ip of M.IpCall { call = c , args } => let 

 val ( ats1 , rts ) = call ( s , e , msg , c ) 

 fun msg' () = msg ( ) ^ ": arguments"  

 val ats2 = operands ( s , e , msg' , args ) 

 val () = case ats1 of NONE => ( )| SOME ats1 => checkConsistentTyps ( s , e , msg' , ats1 , ats2 )  in rts end| M.IpEval { typ , eval = evl } => let 

 val t = eval ( s , e , msg , evl ) 

 fun msg' () = msg ( ) ^ ": thunk value"  

 val () = consistentFieldKind ( s , e , msg' , typ , t )  in SOME ( Vector.new1 t ) end  

 fun cuts ( s , e , msg , M.C { exits , targets } ) = let 

 val () = LS.foreach ( targets , fn l => labelUseNoArgs ( s , e , msg , l ) )  in ( ) end  

 fun return ( s , e , msg , r , ts ) = case r of M.RNormal { rets , block , cuts = cs } => let 

 val () = case ts of NONE => ( )| SOME ts => bindVarsTo ( s , e , msg , rets , ts ) 

 fun msg' () = msg ( ) ^ ": ret target"  

 val () = labelUse ( s , e , msg' , block , Vector.new0 ( ) ) 

 fun msg' () = msg ( ) ^ ": cuts"  

 val () = cuts ( s , e , msg' , cs ) 

 val e = bindVars ( s , e , rets , M.VkLocal )  in e end| M.RTail { exits } => let 

 fun msg' () = msg ( ) ^ ": returns"  

 val rts = getRTyps e 

 val () = case ts of NONE => ( )| SOME ts => checkConsistentTyps ( s , e , msg' , rts , ts )  in e end  

 fun checkContTyp ( s , e , msg , t , ts ) = let 

 fun err () = reportError ( s , msg ( ) ^ ": bad continuation type" )  

 fun msg' () = msg ( ) ^ ": cont args"   in case t of M.TAny => ( )| M.TAnyS vs => if vs = MU.ValueSize.ptrSize ( getConfig e ) then ( ) else err ( )| M.TNonRefPtr => ( )| M.TBits vs => if vs = MU.ValueSize.ptrSize ( getConfig e ) then ( ) else err ( )| M.TNone => ( )| M.TContinuation ts' => checkConsistentTyps ( s , e , msg' , ts' , ts )| _ => if MT.Type.subtype ( getConfig e , t , M.TBits ( MU.ValueSize.ptrSize ( getConfig e ) ) ) then ( ) else err ( ) end  

 fun transfer ( s , e , msg , t ) = case t of M.TGoto t => let 

 val () = target ( s , e , fn () => msg ( ) ^ ": target" , t )  in e end| M.TCase sw => let 

 val () = switch ( s , e , msg , sw )  in e end| M.TInterProc { callee , ret , fx } => let 

 fun msg' () = msg ( ) ^ ": callee"  

 val ts = interProc ( s , e , msg' , callee ) 

 fun msg' () = msg ( ) ^ ": return"  

 val e = return ( s , e , msg' , ret , ts )  in e end| M.TReturn os => let 

 fun msg' () = msg ( ) ^ ": returns"  

 val ts = operands ( s , e , msg' , os ) 

 val rts = getRTyps e 

 val () = checkConsistentTyps ( s , e , msg' , rts , ts )  in e end| M.TCut { cont , args , cuts = cs } => let 

 fun msg' () = msg ( ) ^ ": cont"  

 val t = variableUse ( s , e , msg' , cont ) 

 fun msg' () = msg ( ) ^ ": args"  

 val ts = operands ( s , e , msg' , args ) 

 val () = checkContTyp ( s , e , msg , t , ts ) 

 fun msg' () = msg ( ) ^ ": cuts"  

 val () = cuts ( s , e , msg' , cs )  in e end| M.THalt opnd => let 

 fun msg' () = msg ( ) ^ ": exit code"  

 val t = operand ( s , e , msg' , opnd ) 

 fun badTyp () = reportError ( s , msg ( ) ^ ": exit code not SIntp" )  

 val () = case t of M.TNumeric ( MP.NtInteger ( MP.IpFixed t ) ) => if IntArb.equalTyps ( t , MU.Sintp.intArbTyp ( getConfig e ) ) then ( ) else badTyp ( )| _ => badTyp ( )  in e end  

 fun block ( s , e , l , M.B { parameters , instructions , transfer = t } ) = let 

 fun msgi j () = "label " ^ I.labelString l ^ " instruction " ^ Int.toString j  

 val e = bindVars ( s , e , parameters , M.VkLocal ) 

 fun doOne ( j , i , e ) = instruction ( s , e , msgi j , i )  

 val e = Vector.foldi ( instructions , e , doOne ) 

 fun msg' () = "label " ^ I.labelString l ^ " transfer"  

 val e = transfer ( s , e , msg' , t )  in e end  

 fun blockTree ( s , e , Tree.T ( ( l , b ) , trees ) ) = let 

 val e = block ( s , e , l , b )  in blockForest ( s , e , trees ) end and blockForest ( s , e , trees ) = Vector.foreach ( trees , fn tree => blockTree ( s , e , tree ) )  

 fun codeBody ( s , e , msg , b as M.CB { entry , blocks } ) = let 

 fun doLabel ( l , M.B { parameters , ... } , e ) = let 

 val ts = Vector.map ( parameters , fn x => variable ( s , e , x ) ) 

 val e = bindLabel ( s , e , l , ts )  in e end  

 val e = LD.fold ( blocks , e , doLabel ) 

 val cfg = MilCfg.build ( getConfig e , getSi e , b ) 

 val domtree = MilCfg.getLabelBlockDomTree cfg 

 val () = blockTree ( s , e , domtree ) 

 val () = label ( s , e , entry )  in ( ) end  

 fun code ( s , e , msg , f ) = let 

 val M.F { fx , escapes , recursive , cc , args , rtyps , body } = f 

 fun doOne ( s , e , v ) = bindVar ( s , e , v , M.VkLocal )  

 val e = callConvE ( s , e , doOne , cc ) 

 val cct = MU.CallConv.map ( cc , fn v => variable ( s , e , v ) ) 

 val e = bindVars ( s , e , args , M.VkLocal ) 

 val atyps = variables ( s , e , args ) 

 fun doOne ( i , t ) = typ ( s , e , fn () => msg ( ) ^ ": return " ^ Int.toString i , t )  

 val () = Vector.foreachi ( rtyps , doOne ) 

 val e = setRTyps ( e , rtyps ) 

 val () = codeBody ( s , e , msg , body ) 

 val t = M.TCode { cc=cct , args=atyps , ress=rtyps }  in t end  

 fun index ( s , e , msg , nis ) = let 

 val maxIdx = ND.size nis 

 fun checkOne ( n , idx , idxs ) = let 

 fun msg1 () = msg ( ) ^ ": " ^ I.nameString' n  

 fun msg2 () = msg ( ) ^ ": " ^ Int.toString idx  

 val () = name ( s , e , n ) 

 val () = assert ( s , 0 <= idx andalso idx < maxIdx , fn () => msg1 ( ) ^ ": index out of range" ) 

 val () = assert ( s , not ( IntSet.member ( idxs , idx ) ) , fn () => msg2 ( ) ^ ": index repated" ) 

 val idxs = IntSet.insert ( idxs , idx )  in idxs end  

 val _ = ND.fold ( nis , IntSet.empty , checkOne )  in M.TIdx end  

 fun codeTypToClosureTyp ( s , e , msg , x , t ) = case t of M.TCode { cc = M.CcClosure { cls , fvs } , args , ress } => ( case cls of M.TClosure { args = ats2 , ress = rts2 } => let 

 val () = assert ( s , Vector.length args = Vector.length ats2 , fn () => msg ( ) ^ ": argument number mismatch" ) 

 val () = assert ( s , Vector.length ress = Vector.length rts2 , fn () => msg ( ) ^ ": result number mismatch" ) 

 fun checkOne ( i , t1 , t2 ) = if MT.Type.equal ( t1 , t2 ) then ( ) else reportError ( s , msg ( ) ^ ": arg " ^ Int.toString i ^ ": type mismatch" )  

 val () = if Vector.length args = Vector.length ats2 then Vector.foreachi2 ( args , ats2 , checkOne ) else ( ) 

 fun checkOne ( i , t1 , t2 ) = if MT.Type.equal ( t1 , t2 ) then ( ) else reportError ( s , msg ( ) ^ ": res " ^ Int.toString i ^ ": type mismatch" )  

 val () = if Vector.length ress = Vector.length rts2 then Vector.foreachi2 ( ress , rts2 , checkOne ) else ( )  in M.TClosure { args=args , ress=ress } end| _ => M.TClosure { args=args , ress=ress } )| _ => let 

 val () = reportError ( s , msg ( ) ^ ": not code type" )  in getTyp ( e , x ) end  

 fun global ( s , e , x , g ) = let 

 fun msg () = "global " ^ I.variableString' x  

 val t = case g of M.GCode f => code ( s , e , msg , f )| M.GErrorVal t => let 

 val () = typ ( s , e , msg , t )  in t end| M.GIdx nis => index ( s , e , msg , nis )| M.GTuple { mdDesc , inits } => let 

 val M.MDD { fixed , array , ... } = mdDesc 

 val t = tupleMake ( s , e , msg , mdDesc , inits ) 

 val fixedLen = Vector.length fixed 

 fun badLen () = reportError ( s , msg ( ) ^ ": bad length init" )  

 val varLen = case array of NONE => 0| SOME ( lenIdx , _ ) => case Vector.sub ( inits , lenIdx ) of M.SConstant ( M.CIntegral i ) => IntInf.toInt ( IntArb.toIntInf i )| _ => let 

 val () = badLen ( )  in 0 end 

 val () = assert ( s , Vector.length inits = fixedLen + varLen , fn () => msg ( ) ^ ": wrong number of inits" )  in t end| M.GRat r => MUP.NumericTyp.tRat| M.GInteger i => MUP.NumericTyp.tIntegerArbitrary| M.GCString _ => M.TCString| M.GThunkValue { typ , ofVal } => let 

 fun msg' () = msg ( ) ^ ": thunk value"  

 val t = simple ( s , e , msg' , ofVal ) 

 val () = consistentFieldKind ( s , e , msg' , typ , t ) 

 val t = M.TThunk t  in t end| M.GSimple simp => simple ( s , e , msg , simp )| M.GClosure { code , fvs } => let 

 fun doOne ( i , ( fk , opnd ) ) = let 

 fun msg' () = msg ( ) ^ ": free variable " ^ Int.toString i  

 val t = operand ( s , e , msg' , opnd ) 

 val () = consistentFieldKind ( s , e , msg' , fk , t )  in ( ) end  

 val () = Vector.foreachi ( fvs , doOne ) 

 val t = ( case code of NONE => getTyp ( e , x )| SOME v => let 

 fun msg' () = msg ( ) ^ ": code"  

 val ct = variableUse ( s , e , msg' , v ) 

 val t = codeTypToClosureTyp ( s , e , msg' , x , ct )  in t end )  in t end| M.GSum { tag , typs , ofVals } => let 

 val tTag = sumTag ( s , e , msg , tag ) 

 fun msg' () = msg ( ) ^ ": carried value"  

 val ts = simples ( s , e , msg' , ofVals ) 

 val () = sameLength ( s , e , msg' , typs , ofVals ) 

 val () = Vector.foreach2 ( typs , ts , fn ( typ , t ) => consistentFieldKind ( s , e , msg' , typ , t ) ) 

 val t = M.TSum { tag=tTag , arms=Vector.new1 ( tag , ts ) }  in t end| M.GPSet simp => let 

 fun msg' () = msg ( ) ^ ": set member"  

 val t = simple ( s , e , msg' , simp ) 

 val () = consistentFieldKind ( s , e , msg' , M.FkRef , t ) 

 val t = M.TPType { kind=M.TkE , over=t }  in t end 

 val () = bindVarTo ( s , e , x , t )  in ( ) end  

 fun externs ( s , e , vs ) = VS.fold ( vs , e , fn ( v , e ) => bindVar ( s , e , v , M.VkExtern ) )  

 fun includeFile ( s , e , M.IF { name , kind , externs = evs } ) = let 

 val () = assert ( s , not ( isIncFile ( s , name ) ) , fn () => "file " ^ name ^ " include more than once" ) 

 val () = addIncFile ( s , name ) 

 val e = externs ( s , e , evs )  in e end  

 fun externGroup ( s , e , M.EG { kind , externs = evs } ) = externs ( s , e , evs )  

 fun consistentEntryTyp ( s , e , t ) = case t of M.TCode { cc = M.CcCode , args , ress } => let 

 val () = assert ( s , Vector.length args = 0 , fn () => "program entry takes arguments" ) 

 val () = assert ( s , Vector.length args = 0 , fn () => "program entry return results" )  in ( ) end| _ => reportError ( s , "program entry not of code type" )  

 fun program' ( config , p as M.P { includes , externs , symbolTable , globals , entry } ) = let 

 val s = stateMk ( ) 

 val ( ord , st ) = ( ( I.nameFromString ( symbolTable , "ord" ) , symbolTable ) handle _ => let 

 val stm = IM.fromExistingAll symbolTable 

 val ord = IM.nameMake ( stm , "ord" ) 

 val st = MU.SymbolTableManager.finish stm  in ( ord , st ) end ) 

 val e = envMk ( config , st , ord ) 

 val e = Vector.fold ( includes , e , fn ( i , e ) => includeFile ( s , e , i ) ) 

 val e = Vector.fold ( externs , e , fn ( i , e ) => externGroup ( s , e , i ) ) 

 val vars = I.listVariables st 

 fun msgVar v () = "variable " ^ I.variableString' v  

 fun checkOne v = typ ( s , e , msgVar v , getTyp ( e , v ) )  

 val () = List.foreach ( vars , checkOne ) 

 val e = bindVarsL ( s , e , VD.domain globals , M.VkGlobal ) 

 val () = VD.foreach ( globals , fn ( x , g ) => global ( s , e , x , g ) ) 

 val et = variableUse ( s , e , fn () => "program entry" , entry ) 

 val () = consistentEntryTyp ( s , e , et ) 

 val failed = stateFinish s  in not failed end  

 fun program ( config , p ) = ( if program' ( config , p ) then ( ) else Fail.fail ( "MilCheck" , "program" , "Mil code not well formed" ) ) handle any => let 

 val () = MilLayout.print ( config , p )  in raise any end   end ; 

