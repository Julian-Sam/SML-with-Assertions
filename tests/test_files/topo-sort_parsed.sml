functor TopoSortF ( structure Dict : DICT structure Set : SET sharing type Dict.key = Dict.key  ) :> sig type key = Dict.key val sort : ( key * 'a ) list * ( key * 'a -> Set.t ) -> ( key * 'a ) list list  end = struct 

 type key = Dict.key structure PLG = PolyLabeledGraph 

 val sort = fn ( nodes , df ) => let 

 val node = fn ( ( v , _ ) , n , map ) => Dict.insert ( map , v , n ) 

 val edges = fn map => let 

 val doNode = fn ( v , x ) => let 

 val n = Option.valOf ( Dict.lookup ( map , v ) ) 

 val deps = Set.toList ( df ( v , x ) ) 

 val doOne = fn v => Option.map ( Dict.lookup ( map , v ) , fn n' => ( n' , n , ( ) ) ) 

 val es = List.keepAllMap ( deps , doOne )  in es end 

 val es = List.concat ( List.map ( nodes , doNode ) )  in es end 

 val ( g , _ ) = PLG.new { nodes=nodes , init=Dict.empty , node=node , edges=edges } 

 val scc = PLG.scc g 

 val scc = List.map ( scc , fn l => List.map ( l , PLG.Node.getLabel ) )  in scc end  end 

