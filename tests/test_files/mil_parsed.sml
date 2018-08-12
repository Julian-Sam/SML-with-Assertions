structure Mil = struct structure VD = Identifier.VariableDict structure VS = Identifier.VariableSet structure ND = Identifier.NameDict structure LS = Identifier.LabelSet structure LD = Identifier.LabelDict 

 type variable = Identifier.variable 

 type name = Identifier.name 

 type label = Identifier.label 

 type effects = Effect.set 

 datatype abiCallConv = AbiCdecl | AbiStdcall 

 datatype 'a callConv = CcCode | CcUnmanaged of abiCallConv | CcClosure of { cls : 'a , fvs : 'a Vector.t } | CcThunk of { thunk : 'a , fvs : 'a Vector.t } 

 datatype typKind = TkI | TkE 

 datatype valueSize = Vs8 | Vs16 | Vs32 | Vs64 | Vs128 | Vs256 | Vs512 | Vs1024 

 datatype fieldVariance = FvReadOnly | FvReadWrite 

 datatype fieldSize = Fs8 | Fs16 | Fs32 | Fs64 structure Prims = MilPrimsF ( struct 

 type fieldSize = fieldSize  end ) 

 datatype constant = CBoolean of bool | CRat of IntInf.t | CInteger of IntInf.t | CName of name | CIntegral of IntArb.t | CFloat of Real32.t | CDouble of Real64.t | CViMask of { descriptor : Prims.vectorDescriptor , elts : bool Vector.t } | CRef of IntInf.t | COptionSetEmpty | CTypePH 

 datatype typ = TAny | TAnyS of valueSize | TNonRefPtr | TRef | TBits of valueSize | TNone | TName | TNumeric of Prims.numericTyp | TBoolean | TViVector of { vectorSize : Prims.vectorSize , elementTyp : typ } | TViMask of Prims.vectorDescriptor | TCode of { cc : typ callConv , args : typ Vector.t , ress : typ Vector.t } | TTuple of { fixed : ( typ * valueSize * fieldVariance ) Vector.t , array : ( typ * valueSize * fieldVariance ) } | TCString | TIdx | TContinuation of typ Vector.t | TThunk of typ | TPAny | TClosure of { args : typ Vector.t , ress : typ Vector.t } | TSum of { tag : typ , arms : ( constant * ( typ Vector.t ) ) Vector.t } | TPType of { kind : typKind , over : typ } | TPRef of typ 

 datatype fieldKind = FkRef | FkBits of fieldSize | FkFloat | FkDouble 

 datatype fieldDescriptor = FD of { kind : fieldKind , alignment : valueSize , var : fieldVariance } 

 datatype tupleDescriptor = TD of { fixed : fieldDescriptor Vector.t , array : fieldDescriptor option } 

 datatype metaDataDescriptor = MDD of { pinned : bool , fixed : fieldDescriptor Vector.t , array : ( int * fieldDescriptor ) option } 

 datatype simple = SVariable of variable | SConstant of constant 

 type operand = simple 

 datatype tupleBase = TbScalar | TbVector 

 datatype vectorIndexKind = VikStrided of int | VikVector 

 datatype fieldIdentifier = FiFixed of int | FiVariable of operand | FiVectorFixed of { descriptor : Prims.vectorDescriptor , mask : operand option , index : int } | FiVectorVariable of { descriptor : Prims.vectorDescriptor , base : tupleBase , mask : operand option , index : operand , kind : vectorIndexKind } 

 datatype tupleField = TF of { tupDesc : tupleDescriptor , tup : variable , field : fieldIdentifier } 

 datatype waitPredicate = WpNull | WpNonNull 

 datatype rhs = RhsSimple of simple | RhsPrim of { prim : Prims.t , createThunks : bool , typs : typ Vector.t , args : operand Vector.t } | RhsTuple of { mdDesc : metaDataDescriptor , inits : operand Vector.t } | RhsTupleSub of tupleField | RhsTupleSet of { tupField : tupleField , ofVal : operand } | RhsTupleCAS of { tupField : tupleField , cmpVal : operand , newVal : operand } | RhsTupleWait of { tupField : tupleField , pred : waitPredicate } | RhsTupleInited of { mdDesc : metaDataDescriptor , tup : variable } | RhsIdxGet of { idx : variable , ofVal : operand } | RhsCont of label | RhsThunkMk of { typ : fieldKind , fvs : fieldKind Vector.t } | RhsThunkInit of { typ : fieldKind , thunk : variable option , fx : effects , code : variable option , fvs : ( fieldKind * operand ) Vector.t } | RhsThunkGetFv of { typ : fieldKind , fvs : fieldKind Vector.t , thunk : variable , idx : int } | RhsThunkValue of { typ : fieldKind , thunk : variable option , ofVal : operand } | RhsThunkGetValue of { typ : fieldKind , thunk : variable } | RhsThunkSpawn of { typ : fieldKind , thunk : variable , fx : effects } | RhsClosureMk of { fvs : fieldKind Vector.t } | RhsClosureInit of { cls : variable option , code : variable option , fvs : ( fieldKind * operand ) Vector.t } | RhsClosureGetFv of { fvs : fieldKind Vector.t , cls : variable , idx : int } | RhsPSetNew of operand | RhsPSetGet of variable | RhsPSetCond of { bool : operand , ofVal : operand } | RhsPSetQuery of operand | RhsEnum of { tag : operand , typ : fieldKind } | RhsSum of { tag : constant , ofVals : operand Vector.t , typs : fieldKind Vector.t } | RhsSumProj of { typs : fieldKind Vector.t , sum : variable , tag : constant , idx : int } | RhsSumGetTag of { typ : fieldKind , sum : variable } 

 datatype instruction = I of { dests : variable vector , n : int , rhs : rhs } 

 datatype target = T of { block : label , arguments : operand Vector.t } 

 datatype selector = SeSum of fieldKind | SeConstant 

 type codes = { possible : VS.t , exhaustive : bool } 

 datatype call = CCode of { ptr : variable , code : codes } | CClosure of { cls : variable , code : codes } | CDirectClosure of { cls : variable , code : variable } 

 datatype eval = EThunk of { thunk : variable , value : bool , code : codes } | EDirectThunk of { thunk : variable , value : bool , code : variable } 

 datatype interProc = IpCall of { call : call , args : operand Vector.t } | IpEval of { typ : fieldKind , eval : eval } 

 datatype cuts = C of { exits : bool , targets : LS.t } 

 datatype return = RNormal of { rets : variable Vector.t , block : label , cuts : cuts } | RTail of { exits : bool } 

 datatype transfer = TGoto of target | TCase of { select : selector , on : operand , cases : ( constant * target ) Vector.t , default : target option } | TInterProc of { callee : interProc , ret : return , fx : effects } | TReturn of operand Vector.t | TCut of { cont : variable , args : operand Vector.t , cuts : cuts } | THalt of operand 

 datatype block = B of { parameters : variable Vector.t , instructions : instruction Vector.t , transfer : transfer } 

 datatype codeBody = CB of { entry : label , blocks : block LD.t } 

 datatype code = F of { fx : effects , escapes : bool , recursive : bool , cc : variable callConv , args : variable Vector.t , rtyps : typ Vector.t , body : codeBody } 

 datatype global = GCode of code | GErrorVal of typ | GIdx of int ND.t | GTuple of { mdDesc : metaDataDescriptor , inits : simple Vector.t } | GRat of Rat.t | GInteger of IntInf.t | GCString of string | GThunkValue of { typ : fieldKind , ofVal : simple } | GSimple of simple | GClosure of { code : variable option , fvs : ( fieldKind * simple ) Vector.t } | GSum of { tag : constant , ofVals : simple Vector.t , typs : fieldKind Vector.t } | GPSet of simple 

 type globals = global VD.t 

 datatype includeKind = IkC | IkTarget 

 datatype includeFile = IF of { name : string , kind : includeKind , externs : VS.t } 

 datatype externGroup = EG of { kind : includeKind , externs : VS.t } 

 datatype variableKind = VkExtern | VkGlobal | VkLocal 

 datatype variableInfo = VI of { typ : typ , kind : variableKind } 

 type symbolTable = variableInfo Identifier.symbolTable 

 type symbolTableManager = variableInfo Identifier.Manager.t 

 type symbolInfo = variableInfo Identifier.SymbolInfo.t 

 datatype t = P of { includes : includeFile Vector.t , externs : externGroup Vector.t , globals : globals , symbolTable : symbolTable , entry : variable }  end 

