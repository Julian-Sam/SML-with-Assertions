functor MkSkewBinomialHeapPQ ( structure OrdKey : ORDKEY  ) : PQUEUE = struct structure Key = OrdKey 

 type key = Key.t 

 fun compare ( ( l , _ ) : key * 'a , ( r , _ ) : key * 'a ) : order = Key.compare ( l , r )  

 datatype 'a btree = Node of { data : key * 'a , extra : ( key * 'a ) list , children : 'a btree list } 

 fun rank ( Node { children , ... } ) = List.length children  

 fun data ( Node { data , ... } ) = data  

 fun link ( t1 as Node { data = x1 , extra = xs1 , children = c1 } , t2 as Node { data = x2 , extra = xs2 , children = c2 } ) = case compare ( x1 , x2 ) of ( LESS | EQUAL ) => Node { data=x1 , extra=xs1 , children=t2 :: c1 }| GREATER => Node { data=x2 , extra=xs2 , children=t1 :: c2 }  

 fun skewLink x ( t1 , t2 ) = let 

 val Node { data = y , extra = ys , children = c } = link ( t1 , t2 )  in case compare ( x , y ) of ( LESS | EQUAL ) => Node { data=x , extra=y :: ys , children=c }| GREATER => Node { data=y , extra=x :: ys , children=c } end  

 type 'a pq = { trees : 'a btree list , min : ( key * 'a ) option , size : int } 

 type 'a t = 'a pq 

 fun empty () = { trees=[ ] , min=NONE , size=0 }  

 fun isEmpty { trees = [ ] , min = NONE , size = 0 } = true  
 | isEmpty _ = false  

 fun size ( q : 'a pq ) = # size q  

 fun singleton x = { trees=[ Node { data=x , extra=[ ] , children=[ ] } ] , min=SOME x , size=1 }  

 val $ = singleton 

 fun optionMin ( ( m , NONE ) | ( NONE , m ) ) = m  
 | optionMin ( SOME l , SOME r ) = ( case compare ( l , r ) of LESS => SOME l| _ => SOME r )  

 fun insTree t ts = case ts of [ ] => [ t ]| t' :: ts' => if rank t < rank t' then t :: ts else insTree ( link ( t , t' ) ) ts'  

 fun mergeTrees ( ( ts , [ ] ) | ( [ ] , ts ) ) = ts  
 | mergeTrees ( t1 :: ts1 , t2 :: ts2 ) = case Int.compare ( rank t1 , rank t2 ) of LESS => t1 :: ( mergeTrees ( ts1 , t2 :: ts2 ) )| EQUAL => insTree ( link ( t1 , t2 ) ) ( mergeTrees ( ts1 , ts2 ) )| GREATER => t2 :: ( mergeTrees ( t1 :: ts1 , ts2 ) )  

 fun normalize [ ] = [ ]  
 | normalize ( t :: ts ) = insTree t ts  

 fun insert x { trees = ts , min = m , size = s } = { trees=case ts of t1 :: t2 :: ts' => if rank t1 = rank t2 then skewLink x ( t1 , t2 ) :: ts' else Node { data=x , extra=[ ] , children=[ ] } :: ts| _ => Node { data=x , extra=[ ] , children=[ ] } :: ts , min=optionMin ( SOME x , m ) , size=s + 1 }  

 fun insertList [ ] ts = ts  
 | insertList ( x :: xs ) ts = insertList xs ( insert x ts )  

 fun fromList l = insertList l ( empty ( ) )  

 val % = fromList 

 fun meld ( { trees = ts1 , min = m1 , size = s1 } , { trees = ts2 , min = m2 , size = s2 } ) = { trees=mergeTrees ( normalize ts1 , normalize ts2 ) , min=optionMin ( m1 , m2 ) , size=s1 + s2 }  

 fun findMin ( { min , ... } : 'a pq ) = min  local 

 fun getMin [ ] = NONE  
 | getMin ( t :: ts ) = case getMin ts of NONE => SOME ( t , ts )| SOME ( t' , ts' ) => ( case compare ( data t , data t' ) of ( LESS | EQUAL ) => SOME ( t , ts )| GREATER => SOME ( t' , t :: ts' ) )  

 fun calculateMin [ ] = NONE  
 | calculateMin ( t :: ts ) = optionMin ( calculateMin ts , SOME ( data t ) )   in 

 fun deleteMin ( q as { trees = ts , min = _ , size = s } ) = case getMin ts of NONE => ( NONE , q )| SOME ( Node { data = x , extra = xs , children = c } , ts' ) => ( SOME x , let 

 val ts'' = mergeTrees ( rev c , normalize ts' )  in insertList xs { trees=ts'' , min=calculateMin ts'' , size=s - ( List.length xs ) - 1 } end )   end  end 

