signature ABS_CORE_EVAL = sig datatype demand = S | L | U of demand list type module type t = module * ANormLazy.ty Identifier.symbolTable type t' = module * demand Identifier.symbolTable val annotate : t -> t' val layoutDemand : 'a * demand -> Layout.t  end functor AbsCoreEvalF ( structure AbsCore : ABS_CORE  ) :> ABS_CORE_EVAL where type module = AbsCore.module = struct structure AC = AbsCore structure Dom = AC.Dom structure I = Identifier structure IM = I.Manager structure AL = ANormLazy structure ACL = AbsCoreLayoutF ( struct structure AbsCore = AC 

 type ty = ANormLazy.ty 

 val layoutTy = ANormLazyLayout.layoutTy  end ) structure L = Layout structure LU = LayoutUtils structure GP = GHCPrimType structure VD = I.VariableDict 

 val passname = "AbsCoreEval" 

 datatype demand = S | L | U of demand list 

 type module = AC.module 

 type ty = AL.ty 

 type t = module * ty I.symbolTable 

 type t' = module * demand I.symbolTable 

 type im = demand IM.t 

 type st = ty I.symbolTable 

 val fail = fn ( f , msg ) => Fail.fail ( "AbsCoreEval" , f , msg ) 

 type 'a env = 'a VD.t 

 val rec layoutDemand : 'a * demand -> L.t = fn ( env , t ) => case t of S => L.str "S"| L => L.str "L"| U ds => L.seq [ L.str "U" , LU.sequence ( "[" , "]" , "" ) ( List.map ( ds , fn d => layoutDemand ( env , d ) ) ) ] structure ADL = AbsCoreLayoutF ( struct structure AbsCore = AC 

 type ty = demand 

 val layoutTy = layoutDemand  end ) 

 val arrTyFrom : AL.ty -> AL.ty = fn ty => TypeRep.newRep_ ( AL.Arr ( ty , TypeRep.newRep_ AL.Data , NONE ) ) 

 val lookup : Dom.t env -> AC.var -> Dom.t = fn env => fn v => case VD.lookup ( env , v ) of SOME x => x| NONE => Dom.top 

 val extend : ( AC.var * 'a ) * 'a env -> 'a env = fn ( ( v , x ) , env ) => VD.insert ( env , v , x ) 

 val extends : ( AC.var * 'a ) list * 'a env -> 'a env = fn ( l , env ) => List.fold ( l , env , extend ) 

 val emptyEnv : 'a env = VD.empty 

 val indent = ref 0 

 val probeDemand : ( Dom.t -> Dom.t ) * AL.ty -> demand = fn ( f , ty ) => let 

 val d = if Dom.isBottom ( f Dom.bottom ) then S else L 

 fun probeTys tys = let 

 val l = List.mapi ( tys , fn ( i , _ ) => List.mapi ( tys , fn ( j , _ ) => if i = j then Dom.bottom else Dom.top ) ) 

 val d = List.map ( l , fn xs => if Dom.isBottom ( f ( Dom.tuple xs ) ) then S else L )  in U d end  

 val d = case ( d , TypeRep.repToBase ty ) of ( S , AL.Arr ( ty , _ , _ ) ) => ( case TypeRep.repToBase ty of AL.Sum [ ( con , tys ) ] => probeTys tys| AL.Prim ( GP.Tuple tys ) => probeTys tys| _ => d )| _ => d  in d end 

 val rec evalExp : im * st * Dom.t env -> AbsCore.exp -> Dom.t = fn ( im , st , env ) => fn e => let 

 val _ = indent := ! indent + 2 

 val x = case e of AC.Var v => lookup env v| AC.Multi vs => Dom.tuple ( List.map ( vs , lookup env ) )| AC.Con ( _ , vs ) => let 

 val us = List.keepAll ( vs , # 2 ) 

 val b = List.exists ( us , Dom.isBottom o ( lookup env ) o # 1 )  in if b then Dom.bottom else Dom.tuple ( List.map ( vs , lookup env o # 1 ) ) end| AC.App ( e , v ) => let 

 val p = evalExp ( im , st , env ) e  in case Dom.isFunc p of SOME g => g ( lookup env v )| NONE => if Dom.isBottom p then Dom.bottom else Dom.top end| AC.Lam ( v , e ) => let 

 val ty = I.variableInfo ( st , v ) 

 val g = fn x => evalExp ( im , st , extend ( ( v , x ) , env ) ) e  in Dom.func g end| AC.Let ( vdefg , e ) => let 

 val ( vs , env ) = evalVDefg ( im , st , env ) vdefg  in evalExp ( im , st , env ) e end| AC.GLB vs => Dom.glb ( List.map ( vs , lookup env ) )| AC.LUB vs => Dom.lub ( List.map ( vs , lookup env ) )| AC.Cond ( e1 , e2 ) => if Dom.isBottom ( evalExp ( im , st , env ) e1 ) then Dom.bottom else evalExp ( im , st , env ) e2| AC.Const x => x 

 val _ = indent := ! indent - 2  in x end and rec evalVDefg : im * st * Dom.t env -> AC.vDefg -> AC.var list * Dom.t env = fn ( im , st , env ) => fn vdefg => let  in case vdefg of AC.Rec vdefs => let 

 val ves = List.map ( vdefs , fn AC.Vdef ( ( AC.VbSingle v ) , e ) => ( v , e )| AC.Vdef _ => fail ( "evalVDefg" , "multi-return in letrec" ) ) 

 val ( vs , es ) = List.unzip ves 

 fun iterate ( xs , ds ) = let 

 val env' = extends ( List.zip ( vs , xs ) , env ) 

 val ( xs' , ds' ) = List.unzip ( List.map ( ves , fn ( v , e ) => widen ( st , v , evalExp ( im , st , env' ) e ) ) )  in if ds = ds' then xs else iterate ( xs' , ds' ) end   in ( vs , extends ( List.zip ( vs , iterate ( List.map ( vs , fn _ => Dom.bottom ) , [ ] ) ) , env ) ) end| AC.Nonrec ( AC.Vdef ( vb , e ) ) => ( case vb of AC.VbSingle v => ( [ v ] , extend ( ( v , evalExp ( im , st , env ) e ) , env ) )| AC.VbMulti ( vs , effectful ) => let 

 val p = if effectful then Dom.top else evalExp ( im , st , env ) e  in ( vs , case Dom.isTuple p of SOME xs => List.fold ( List.zip ( vs , xs ) , env , extend )| NONE => if Dom.isTop p then extends ( List.map ( vs , fn v => ( v , Dom.top ) ) , env ) else if Dom.isBottom p then extends ( List.map ( vs , fn v => ( v , Dom.bottom ) ) , env ) else fail ( "evalVDefg" , "cannot match multi" ) ) end ) end and rec widen : st * AC.var * Dom.t -> Dom.t * demand = fn ( st , v , p ) => case Dom.isFunc p of SOME f => let 

 val d = probeDemand ( f , I.variableInfo ( st , v ) ) 

 fun satisfy ( x , d ) = case d of L => false| S => Dom.isBottom x| U ds => Dom.isBottom x orelse ( case Dom.isTuple x of SOME xs => List.exists ( List.zip ( xs , ds ) , satisfy )| NONE => false )   in ( Dom.func ( fn x => if satisfy ( x , d ) then Dom.bottom else Dom.top ) , d ) end| NONE => ( p , L ) 

 val rec fromDemand : demand -> Dom.t = fn S => Dom.top| L => Dom.bottom| U ds => Dom.tuple ( List.map ( ds , fromDemand ) ) 

 val rec check : Dom.t * Dom.t -> Dom.t = fn ( u , v ) => let 

 fun satisfy ( u , v ) = Dom.isBottom v orelse ( if Dom.isTop v then not ( Dom.isBottom u ) else case Dom.isFunc v of SOME _ => true| NONE => ( case ( Dom.isTuple u , Dom.isTuple v ) of ( SOME u , SOME v ) => ( List.length u <> List.length v ) orelse List.forall ( List.zip ( u , v ) , satisfy )| ( NONE , SOME v ) => Dom.isTop u| _ => fail ( "check" , "impossible domain value" ) ) )   in if satisfy ( u , v ) then u else Dom.bottom end 

 val getExpect : im -> AC.var -> Dom.t = fn im => fn v => if IM.variableHasInfo ( im , v ) then fromDemand ( IM.variableInfo ( im , v ) ) else Dom.bottom 

 val rec probeExp : im * st * Dom.t env * Dom.t -> AbsCore.exp -> unit = fn ( im , st , env , expect ) => fn e => let  in case e of AC.App ( e , v ) => probeExp ( im , st , env , Dom.func ( fn _ => expect ) ) e| AC.Lam ( v , e ) => let 

 val ty = I.variableInfo ( st , v ) 

 val expect = case Dom.isFunc expect of SOME f => f Dom.bottom| NONE => Dom.bottom 

 val f = fn x => check ( evalExp ( im , st , extend ( ( v , x ) , env ) ) e , expect ) 

 val () = IM.variableSetInfo ( im , v , probeDemand ( f , arrTyFrom ty ) )  in probeExp ( im , st , env , expect ) e end| AC.Let ( vdefg , e ) => let 

 val ( vs , env ) = evalVDefg ( im , st , env ) vdefg 

 val _ = case vs of [ v ] => let 

 val ty = I.variableInfo ( st , v ) 

 val f = fn x => check ( evalExp ( im , st , extend ( ( v , x ) , env ) ) e , expect )  in IM.variableSetInfo ( im , v , probeDemand ( f , arrTyFrom ty ) ) end| _ => let 

 val ty = TypeRep.newRep_ ( AL.Prim ( GP.Tuple ( List.map ( vs , fn v => I.variableInfo ( st , v ) ) ) ) ) 

 val f = fn x => let 

 val env = case Dom.isTuple x of SOME xs => extends ( List.zip ( vs , xs ) , env )| _ => if Dom.isBottom x then extends ( List.map ( vs , fn v => ( v , Dom.bottom ) ) , env ) else env  in check ( evalExp ( im , st , env ) e , expect ) end  in case probeDemand ( f , arrTyFrom ty ) of U ds => List.foreach ( List.zip ( vs , ds ) , fn ( v , d ) => IM.variableSetInfo ( im , v , d ) )| _ => List.foreach ( vs , fn v => IM.variableSetInfo ( im , v , L ) ) end 

 val () = probeExp ( im , st , env , expect ) e  in probeVDefg ( im , st , env , vs ) vdefg end| AC.Cond ( e1 , e2 ) => probeExp ( im , st , env , Dom.bottom ) e1 before probeExp ( im , st , env , expect ) e2| _ => ( ) end and rec probeVDefg : im * st * Dom.t env * AC.var list -> AC.vDefg -> unit = fn ( im , st , env , vs ) => fn AC.Rec vdefs => let 

 val _ = List.map ( vdefs , fn AC.Vdef ( ( AC.VbSingle v ) , e ) => probeExp ( im , st , env , getExpect im v ) e| AC.Vdef _ => fail ( "probeVDefg" , "multi-return in letrec" ) )  in ( ) end| AC.Nonrec ( AC.Vdef ( AC.VbSingle v , e ) ) => probeExp ( im , st , env , getExpect im v ) e| AC.Nonrec ( AC.Vdef ( AC.VbMulti _ , e ) ) => probeExp ( im , st , env , Dom.tuple ( List.map ( vs , getExpect im ) ) ) e 

 val probeVDefg' = fn ( im , st ) => fn ( vdefg , env ) => let 

 val ( vs , env ) = evalVDefg ( im , st , env ) vdefg 

 val () = probeVDefg ( im , st , env , vs ) vdefg  in env end 

 val annotate : t -> t' = fn ( AC.Module ( main , vdefgs ) , st ) => let 

 val im = IM.fromExistingNoInfo st 

 val _ = List.fold ( vdefgs , emptyEnv , probeVDefg' ( im , st ) ) 

 val _ = List.fold ( I.listVariables st , ( ) , fn ( v , _ ) => if IM.variableHasInfo ( im , v ) then ( ) else IM.variableSetInfo ( im , v , L ) )  in ( AC.Module ( main , vdefgs ) , IM.finish im ) end  end 

