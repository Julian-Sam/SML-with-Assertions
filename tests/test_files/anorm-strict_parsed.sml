structure ANormStrict = struct 

 type effect = Effect.set 

 type name = Identifier.name 

 type var = Identifier.variable 

 type con = Identifier.name * int 

 type lit = CoreHs.coreLit 

 type cc = CoreHs.callconv 

 type mname = string 

 type pname = string 

 type 'a primTy = 'a GHCPrimType.primTy 

 datatype ty_ = Boxed | Prim of ty primTy | Arr of ty list * ty list * effect | Sum of ( con * ty list ) list | Thunk of ty withtype ty = ty_ TypeRep.rep 

 type vbinds = ( var * ty ) list 

 datatype cast = FromAddr of var | ToAddr of var | NullRef | Bottom of var 

 datatype exp = Return of var List.t | PrimApp of GHCPrimOp.primOp * var list | ExtApp of pname * cc * string * ty * var list | ConApp of con * var list | App of var * var list * effect | Let of vDefg * exp | Case of var * alt list | Lit of lit * ty | Cast of cast | Eval of var and alt = Acon of con * vbinds * exp | Alit of lit * ty * exp | Adefault of exp and vDef = Vfun of { name : var , ty : ty , escapes : bool , recursive : bool , fvs : var list , args : vbinds , body : exp } | Vthk of { name : var , ty : ty , escapes : bool , recursive : bool , fvs : var list , body : exp } and vDefg = Rec of vDef list | Nonrec of vDef | Vdef of ( var * ty ) list * exp 

 type tDef = var * ty 

 datatype variableKind = VkGlobal | VkLocal 

 type symbolTable = ( ty * variableKind ) Identifier.symbolTable 

 type symbolInfo = ( ty * variableKind ) Identifier.SymbolInfo.t 

 type symbolTableManager = ( ty * variableKind ) Identifier.Manager.t 

 type typeManager = ty_ TypeRep.manager 

 datatype module = Module of var * vDefg list 

 type t = module * symbolTable * typeManager 

 val eqTy_ : ty_ TypeRep.baseEq = fn f => fn ( x , y ) => let 

 fun both ( m , n , h ) = List.length m = List.length n andalso List.forall ( List.zip ( m , n ) , h )   in case ( x , y ) of ( Boxed , Boxed ) => true| ( Prim a , Prim b ) => GHCPrimType.eqPrimTy f ( a , b )| ( Arr ( a , b , c ) , Arr ( u , v , w ) ) => both ( a , u , f ) andalso both ( b , v , f ) andalso c = w| ( Sum xs , Sum ys ) => let 

 fun g ( ( ( u , i ) , l ) , ( ( v , j ) , m ) ) = u = v andalso i = j andalso both ( l , m , f )   in both ( xs , ys , g ) end| ( Thunk a , Thunk u ) => f ( a , u )| _ => false end 

 val hashTy_ : ty_ TypeRep.baseHash = fn hashRep => fn t => ( case t of Boxed => 0w1| Prim p => TypeRep.hash2 ( 0w2 , GHCPrimType.hashPrimTy hashRep p )| Arr ( t1 , t2 , _ ) => TypeRep.hash3 ( 0w3 , TypeRep.hashList ( List.map ( t1 , hashRep ) ) , TypeRep.hashList ( List.map ( t2 , hashRep ) ) )| Sum arms => let 

 fun doArm ( ( _ , i ) , tys ) = TypeRep.hash2 ( Word.fromInt i , TypeRep.hashList ( List.map ( tys , hashRep ) ) )   in TypeRep.hash2 ( 0w4 , TypeRep.hashList ( List.map ( arms , doArm ) ) ) end| Thunk t => TypeRep.hash2 ( 0w5 , hashRep t ) )  end 

