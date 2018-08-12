signature ANORM_STRICT_UTILS = sig structure VDef : sig val binder : ANormStrict.vDef -> ANormStrict.var * ANormStrict.ty val binders : ANormStrict.vDef List.t -> ( ANormStrict.var * ANormStrict.ty ) List.t val variableDefd : ANormStrict.vDef -> ANormStrict.var val variablesDefd : ANormStrict.vDef List.t -> ANormStrict.var List.t  end structure VDefg : sig val binder : ANormStrict.vDefg -> ( ANormStrict.var * ANormStrict.ty ) List.t val binders : ANormStrict.vDefg List.t -> ( ANormStrict.var * ANormStrict.ty ) List.t val variableDefd : ANormStrict.vDefg -> ANormStrict.var List.t val variablesDefd : ANormStrict.vDefg List.t -> ANormStrict.var List.t  end  end structure ANormStrictUtils :> ANORM_STRICT_UTILS = struct structure AS = ANormStrict structure VDef = struct 

 val binder : ANormStrict.vDef -> ANormStrict.var * ANormStrict.ty = fn vd => ( case vd of AS.Vfun { name , ty , ... } => ( name , ty )| AS.Vthk { name , ty , ... } => ( name , ty ) ) 

 val binders : ANormStrict.vDef List.t -> ( ANormStrict.var * ANormStrict.ty ) List.t = fn vds => List.map ( vds , binder ) 

 val variableDefd : ANormStrict.vDef -> ANormStrict.var = # 1 o binder 

 val variablesDefd : ANormStrict.vDef List.t -> ANormStrict.var List.t = fn vds => List.map ( vds , variableDefd )  end structure VDefg = struct 

 val binder : ANormStrict.vDefg -> ( ANormStrict.var * ANormStrict.ty ) List.t = fn vdg => ( case vdg of AS.Vdef ( vts , e ) => vts| AS.Nonrec vd => VDef.binders [ vd ]| AS.Rec vds => VDef.binders vds ) 

 val binders : ANormStrict.vDefg List.t -> ( ANormStrict.var * ANormStrict.ty ) List.t = fn vds => List.concatMap ( vds , binder ) 

 val variableDefd : ANormStrict.vDefg -> ANormStrict.var List.t = fn vd => List.map ( binder vd , # 1 ) 

 val variablesDefd : ANormStrict.vDefg List.t -> ANormStrict.var List.t = fn vds => List.concatMap ( vds , variableDefd )  end  end 

