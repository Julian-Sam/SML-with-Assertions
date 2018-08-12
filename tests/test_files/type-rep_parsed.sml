signature TYPE_REP = sig type 'base rep type 'base manager type 'base baseHash = ( 'base rep -> word ) -> 'base -> word type 'base baseEq = ( 'base rep * 'base rep -> bool ) -> 'base * 'base -> bool val hashRep : ( 'base -> word ) -> 'base rep -> word val hashRepWithManager : 'base manager * 'base rep -> word val newManager : 'base baseHash * 'base baseEq -> 'base manager val size : 'base manager -> int val newRep : 'base manager * 'base -> 'base rep val newRep_ : 'base -> 'base rep val repToBase : 'base rep -> 'base val hash2 : word * word -> word val hash3 : word * word * word -> word val hash4 : word * word * word * word -> word val hashList : word list -> word  end structure TypeRep : TYPE_REP = struct structure H = HashTable structure UO = Utils.Option  exception NotFound 

 datatype 'base rep = Ty of { base : 'base , meta : { hash : word , uid : word } option } 

 datatype 'base manager = Tbl of { stamp : word ref , table : ( 'base , 'base rep ) H.hash_table , hashBase : 'base -> word } 

 type 'base baseHash = ( 'base rep -> word ) -> 'base -> word 

 type 'base baseEq = ( 'base rep * 'base rep -> bool ) -> 'base * 'base -> bool 

 val hashRep : ( 'base -> word ) -> 'base rep -> word = fn hashBase => fn Ty { base , meta } => UO.dispatch ( meta , fn { hash , ... } => hash , fn () => hashBase base ) 

 val hashRepWithManager : 'base manager * 'base rep -> word = fn ( Tbl { hashBase , ... } , x ) => hashRep hashBase x 

 val eqRep : ( 'base * 'base -> bool ) -> 'base rep * 'base rep -> bool = fn eqBase => fn ( Ty { base = x , meta = u } , Ty { base = y , meta = v } ) => ( case ( u , v ) of ( SOME u , SOME v ) => # uid u = # uid v| _ => eqBase ( x , y ) ) 

 val newManager : 'base baseHash * 'base baseEq -> 'base manager = fn ( hash , eq ) => let 

 fun hashBase x = hash ( hashRep hashBase ) x  

 fun eqBase x = eq ( eqRep eqBase ) x  

 val table = H.mkTable ( hashBase , eqBase ) ( 32 , NotFound )  in Tbl { stamp=ref 0w0 , table=table , hashBase=hashBase } end 

 val size : 'base manager -> int = fn Tbl tbl => H.numItems ( # table tbl ) 

 val newRep : 'base manager * 'base -> 'base rep = fn ( tbl as Tbl { stamp , table , hashBase } , base ) => UO.dispatch ( H.find table base , fn x => x , fn () => let 

 val () = stamp := Word.+ ( ! stamp , 0w1 ) 

 val x = Ty { base=base , meta=SOME { hash=hashBase base , uid=! stamp } } 

 val () = H.insert table ( base , x )  in x end ) 

 val newRep_ : 'base -> 'base rep = fn b => Ty { base=b , meta=NONE } 

 val repToBase : 'base rep -> 'base = fn Ty x => # base x 

 fun hash2 ( a , b ) = Word.+ ( Word.* ( a , 0wx133 ) , b )  

 fun hash3 ( a , b , c ) = hash2 ( a , hash2 ( b , c ) )  

 fun hash4 ( a , b , c , d ) = hash2 ( a , hash2 ( b , hash2 ( c , d ) ) )  

 fun hashList l = List.fold ( l , 0wx0 , hash2 )   end 

