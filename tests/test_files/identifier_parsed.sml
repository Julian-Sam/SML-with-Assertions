signature IDENTIFIER = sig eqtype variable eqtype name eqtype label type 'a symbolTable structure VariableSet : SET where type element = variable structure VariableDict : DICT where type key = variable structure NameSet : SET where type element = name structure NameDict : DICT where type key = name structure LabelSet : SET where type element = label structure LabelLabelSet : SET where type element = label * label structure LabelDict : DICT where type key = label structure ImpVariableDict : DICT_IMP where type key = variable structure ImpNameDict : DICT_IMP where type key = name structure ImpLabelDict : DICT_IMP where type key = label val variableExists : 'a symbolTable * variable -> bool val variableCompare : variable * variable -> order val variableEqual : variable * variable -> bool val variableNumber : variable -> int val variableLookup : 'a symbolTable * variable -> string * 'a val variableName : 'a symbolTable * variable -> string val variableNameEscaped : 'a symbolTable * variable -> string val variableInfo : 'a symbolTable * variable -> 'a val variableString' : variable -> string val variableString : 'a symbolTable * variable -> string val variableStringEscaped : 'a symbolTable * variable -> string val variableTopoSort : ( variable * 'a ) list * ( variable * 'a -> VariableSet.t ) -> ( variable * 'a ) list list val nameExists : 'a symbolTable * name -> bool val nameCompare : name * name -> order val nameEqual : name * name -> bool val nameNumber : name -> int val nameString' : name -> string val nameString : 'a symbolTable * name -> string val nameStringEscaped : 'a symbolTable * name -> string val nameFromString : 'a symbolTable * string -> name val labelExists : 'a symbolTable * label -> bool val labelCompare : label * label -> order val labelEqual : label * label -> bool val labelNumber : label -> int val labelString : label -> string val layoutVariable' : variable -> Layout.t val layoutVariable : variable * 'a symbolTable -> Layout.t val layoutVariableEscaped : variable * 'a symbolTable -> Layout.t val layoutName' : name -> Layout.t val layoutName : name * 'a symbolTable -> Layout.t val layoutNameEscaped : name * 'a symbolTable -> Layout.t val layoutLabel : label -> Layout.t val listVariables : 'a symbolTable -> variable list val listNames : 'a symbolTable -> name list structure Manager : sig type 'a t val new : string -> 'a t val fromExistingAll : 'a symbolTable -> 'a t val fromExistingAll' : 'a t -> 'a t val fromExistingNoInfo : 'a symbolTable -> 'b t val fromExistingNoInfo' : 'a t -> 'b t val variableExists : 'a t * variable -> bool val variableLookup : 'a t * variable -> string * 'a val variableName : 'a t * variable -> string val variableNameEscaped : 'a t * variable -> string val variableInfo : 'a t * variable -> 'a val variableString : 'a t * variable -> string val variableStringEscaped : 'a t * variable -> string val nameExists : 'a t * name -> bool val nameString : 'a t * name -> string val nameStringEscaped : 'a t * name -> string val nameFromString : 'a t * string -> name val variableFresh : 'a t * string * 'a -> variable val variableFreshNoInfo : 'a t * string -> variable val variableClone : 'a t * variable -> variable val variableRelated : 'a t * variable * string * 'a -> variable val variableRelatedNoInfo : 'a t * variable * string -> variable val variableHasInfo : 'a t * variable -> bool val variableSetInfo : 'a t * variable * 'a -> unit val variableDelete : 'a t * variable -> unit val variablesList : 'a t -> variable list val nameMake : 'a t * string -> name val labelFresh : 'a t -> label val finish : 'a t -> 'a symbolTable val finishMapFold : 'a t * 'b * ( variable * 'a * 'b -> 'c * 'b ) -> 'c symbolTable * 'b val layoutVariable : variable * 'a t -> Layout.t val layoutVariableEscaped : variable * 'a t -> Layout.t val layoutName : name * 'a t -> Layout.t val layoutNameEscaped : name * 'a t -> Layout.t  end structure SymbolInfo : sig datatype 'a t = SiTable of 'a symbolTable | SiManager of 'a Manager.t val variableExists : 'a t * variable -> bool val variableLookup : 'a t * variable -> string * 'a val variableName : 'a t * variable -> string val variableNameEscaped : 'a t * variable -> string val variableInfo : 'a t * variable -> 'a val variableString : 'a t * variable -> string val variableStringEscaped : 'a t * variable -> string val nameExists : 'a t * name -> bool val nameString : 'a t * name -> string val nameStringEscaped : 'a t * name -> string val nameFromString : 'a t * string -> name val layoutVariable : variable * 'a t -> Layout.t val layoutVariableEscaped : variable * 'a t -> Layout.t val layoutName : name * 'a t -> Layout.t val layoutNameEscaped : name * 'a t -> Layout.t val layoutLabel : label * 'a t -> Layout.t  end  end structure Identifier :> IDENTIFIER = struct 

 datatype variable = V of int 

 datatype name = N of int 

 datatype label = L of int 

 fun layoutVariable' ( V n ) = Layout.seq [ Layout.str "v" , Int.layout n ]  

 fun labelString ( L n ) = "L" ^ ( Int.toString n )  

 fun layoutLabel l = Layout.str ( labelString l )  

 fun variableCompare ( V v1 , V v2 ) = Int.compare ( v1 , v2 )  

 fun variableEqual ( v1 , v2 ) = variableCompare ( v1 , v2 ) = EQUAL  

 fun nameCompare ( N n1 , N n2 ) = Int.compare ( n1 , n2 )  

 fun nameEqual ( n1 , n2 ) = nameCompare ( n1 , n2 ) = EQUAL  

 fun labelCompare ( L l1 , L l2 ) = Int.compare ( l1 , l2 )  

 fun labelEqual ( l1 , l2 ) = labelCompare ( l1 , l2 ) = EQUAL  local structure Ord = struct 

 type t = variable 

 val compare = variableCompare  end  in structure VariableSet = SetF ( Ord ) structure VariableDict = DictF ( Ord ) structure ImpVariableDict = DictImpF ( Ord )  end local structure Ord = struct 

 type t = name 

 val compare = nameCompare  end  in structure NameSet = SetF ( Ord ) structure NameDict = DictF ( Ord ) structure ImpNameDict = DictImpF ( Ord )  end local structure Ord = struct 

 type t = label 

 val compare = labelCompare  end  in structure LabelSet = SetF ( Ord ) structure LabelLabelSet = SetF ( struct 

 type t = label * label 

 val compare = Compare.pair ( labelCompare , labelCompare )  end ) structure LabelDict = DictF ( Ord ) structure ImpLabelDict = DictImpF ( Ord )  end 

 datatype 'a symbolTable = S of { variableNames : string VariableDict.t , variableInfo : 'a VariableDict.t , names : string NameDict.t , rnames : name StringDict.t , maxVariable : int , maxName : int , maxLabel : int } 

 fun variableExists ( S { variableNames , ... } , v ) = VariableDict.contains ( variableNames , v )  

 fun variableNumber ( V n ) = n  

 fun variableName ( S { variableNames , ... } , v ) = case VariableDict.lookup ( variableNames , v ) of NONE => Fail.fail ( "Identifier" , "variableName" , "undefined: " ^ ( Layout.toString ( layoutVariable' v ) ) )| SOME s => s  

 fun doEscaping s = let 

 fun doOne c = if Char.isAlphaNum c orelse String.contains ( "_\\-$" , c ) then String.fromChar c else let 

 val ord = Char.ord c 

 val d1 = ord mod 16 

 val d2 = ord div 16 

 val s = String.implode [ #"^" , Char.fromHexDigit d2 , Char.fromHexDigit d1 ]  in s end  

 val s = String.translate ( s , doOne )  in s end  

 fun variableNameEscaped ( st , v ) = doEscaping ( variableName ( st , v ) )  

 fun variableInfo ( S { variableInfo , ... } , v ) = case VariableDict.lookup ( variableInfo , v ) of NONE => Fail.fail ( "Identifier" , "variableInfo" , "undefined: " ^ ( Layout.toString ( layoutVariable' v ) ) )| SOME i => i  

 fun variableLookup ( st , v ) = ( variableName ( st , v ) , variableInfo ( st , v ) )  

 fun variableString ( st , v as V n ) = let 

 val str = "_" ^ variableName ( st , v ) handle _ => "" 

 val str = "v" ^ Int.toString n ^ str  in str end  

 fun variableStringEscaped ( st , v as V n ) = let 

 val str = "_" ^ variableNameEscaped ( st , v ) handle _ => "" 

 val str = "v" ^ ( Int.toString n ) ^ str  in str end  

 fun variableString' v = "v" ^ ( Int.toString ( variableNumber v ) )  local structure TopoSort = TopoSortF ( struct structure Dict = VariableDict structure Set = VariableSet  end )  in 

 val variableTopoSort = TopoSort.sort  end 

 fun nameExists ( S { names , ... } , n ) = NameDict.contains ( names , n )  

 fun nameNumber ( N n ) = n  

 fun nameString' n = "n" ^ ( Int.toString ( nameNumber n ) )  

 fun nameString ( S { names , ... } , n ) = case NameDict.lookup ( names , n ) of NONE => Fail.fail ( "Identifier" , "nameString" , "undefined" )| SOME s => s  

 fun nameStringEscaped ( st , n ) = doEscaping ( nameString ( st , n ) )  

 fun nameFromString ( S { rnames , ... } , s ) = case StringDict.lookup ( rnames , s ) of NONE => Fail.fail ( "Identifier" , "nameFromString" , "undefined" )| SOME n => n  

 fun labelExists ( S { maxLabel , ... } , L n ) = 0 <= n andalso n < maxLabel  

 fun labelNumber ( L n ) = n  

 fun layoutName' ( N n ) = Layout.seq [ Layout.str "n" , Int.layout n ]  

 fun layoutVariable ( v , st ) = Layout.str ( variableString ( st , v ) )  

 fun layoutVariableEscaped ( v , st ) = Layout.str ( variableStringEscaped ( st , v ) )  

 fun layoutName ( n as N m , st ) = let 

 val str = "_" ^ nameString ( st , n ) handle _ => "" 

 val l = Layout.str ( "n" ^ ( Int.toString m ) ^ str )  in l end  

 fun layoutNameEscaped ( n as N m , st ) = let 

 val str = "_" ^ nameStringEscaped ( st , n ) handle _ => "" 

 val l = Layout.str ( "n" ^ ( Int.toString m ) ^ str )  in l end  

 fun listVariables ( S { variableNames , ... } ) = let 

 val vns = VariableDict.toList variableNames 

 val vs = List.map ( vns , # 1 )  in vs end  

 fun listNames ( S { names , ... } ) = List.map ( NameDict.toList names , # 1 )  structure Manager = struct 

 datatype 'a t = M of { variable : int ref , name : int ref , label : int ref , variableNames : string VariableDict.t ref , variableInfo : 'a VariableDict.t ref , names : string NameDict.t ref , rnames : name StringDict.t ref } 

 fun fromExistingAll ( S st ) = M { variable=ref ( # maxVariable st ) , name=ref ( # maxName st ) , label=ref ( # maxLabel st ) , variableNames=ref ( # variableNames st ) , variableInfo=ref ( # variableInfo st ) , names=ref ( # names st ) , rnames=ref ( # rnames st ) }  

 fun fromExistingAll' ( M stm ) = M { variable=ref ( ! ( # variable stm ) ) , name=ref ( ! ( # name stm ) ) , label=ref ( ! ( # label stm ) ) , variableNames=ref ( ! ( # variableNames stm ) ) , variableInfo=ref ( ! ( # variableInfo stm ) ) , names=ref ( ! ( # names stm ) ) , rnames=ref ( ! ( # rnames stm ) ) }  

 fun fromExistingNoInfo ( S st ) = M { variable=ref ( # maxVariable st ) , name=ref ( # maxName st ) , label=ref ( # maxLabel st ) , variableNames=ref ( # variableNames st ) , variableInfo=ref VariableDict.empty , names=ref ( # names st ) , rnames=ref ( # rnames st ) }  

 fun fromExistingNoInfo' ( M stm ) = M { variable=ref ( ! ( # variable stm ) ) , name=ref ( ! ( # name stm ) ) , label=ref ( ! ( # label stm ) ) , variableNames=ref ( ! ( # variableNames stm ) ) , variableInfo=ref VariableDict.empty , names=ref ( ! ( # names stm ) ) , rnames=ref ( ! ( # rnames stm ) ) }  

 fun variableExists ( M { variableNames , ... } , v ) = VariableDict.contains ( ! variableNames , v )  

 fun variableName ( M { variableNames , ... } , v ) = case VariableDict.lookup ( ! variableNames , v ) of NONE => Fail.fail ( "Identifier.Manager" , "variableName" , "undefined" )| SOME s => s  

 fun variableNameEscaped ( stm , v ) = doEscaping ( variableName ( stm , v ) )  

 fun variableInfo ( M { variableInfo , variableNames , ... } , v as V n ) = case VariableDict.lookup ( ! variableInfo , v ) of NONE => let 

 val str = case VariableDict.lookup ( ! variableNames , v ) of NONE => ""| SOME s => "_" ^ s 

 val name = "v" ^ ( Int.toString n ) ^ str  in Fail.fail ( "Identifier.Manager" , "variableInfo" , "undefined " ^ name ) end| SOME i => i  

 fun variableLookup ( stm , v ) = ( variableName ( stm , v ) , variableInfo ( stm , v ) )  

 fun variableString ( stm , v as V n ) = let 

 val str = "_" ^ variableName ( stm , v ) handle _ => "" 

 val str = "v" ^ Int.toString n ^ str  in str end  

 fun variableStringEscaped ( stm , v as V n ) = let 

 val str = "_" ^ variableNameEscaped ( stm , v ) handle _ => "" 

 val str = "v" ^ Int.toString n ^ str  in str end  

 fun variablesList ( M { variableNames , ... } ) = List.map ( VariableDict.toList ( ! variableNames ) , # 1 )  

 fun nameExists ( M { names , ... } , n ) = NameDict.contains ( ! names , n )  

 fun nameString ( M { names , ... } , n ) = case NameDict.lookup ( ! names , n ) of NONE => Fail.fail ( "Identifier.Manager" , "nameString" , "undefined" )| SOME s => s  

 fun nameStringEscaped ( stm , n ) = doEscaping ( nameString ( stm , n ) )  

 fun nameFromString ( M { rnames , ... } , s ) = case StringDict.lookup ( ! rnames , s ) of NONE => Fail.fail ( "Identifier.Manager" , "nameFromString" , "undefined" )| SOME n => n  

 fun variableFreshNoInfo ( M { variable , variableNames , ... } , hint ) = let 

 val nv = ! variable 

 val () = variable := nv + 1 

 fun doOne c = if c = #"#" then Int.toString nv else String.fromChar c  

 val nm = String.translate ( hint , doOne ) 

 val nv = V nv 

 val () = variableNames := VariableDict.insert ( ! variableNames , nv , nm )  in nv end  

 fun variableHasInfo ( M { variableInfo , ... } , v ) = VariableDict.contains ( ! variableInfo , v )  

 fun variableSetInfo ( M { variableNames , variableInfo , ... } , v , info ) = case VariableDict.lookup ( ! variableNames , v ) of NONE => Fail.fail ( "Identifier.Manager" , "variableSetInfo" , "undefined" )| SOME _ => variableInfo := VariableDict.insert ( ! variableInfo , v , info )  

 fun variableFresh ( stm , hint , info ) = let 

 val nv = variableFreshNoInfo ( stm , hint ) 

 val () = variableSetInfo ( stm , nv , info )  in nv end  

 fun variableClone ( stm as M { variable , variableNames , variableInfo , ... } , v ) = let 

 val nv = ! variable 

 val () = variable := nv + 1 

 val nv = V nv 

 val () = case VariableDict.lookup ( ! variableNames , v ) of NONE => Fail.fail ( "Identifier.Manager" , "variableClone" , variableString' v ^ " has no name" )| SOME nm => variableNames := VariableDict.insert ( ! variableNames , nv , nm ) 

 val () = case VariableDict.lookup ( ! variableInfo , v ) of NONE => ( )| SOME info => variableInfo := VariableDict.insert ( ! variableInfo , nv , info )  in nv end  

 fun variableRelatedNoInfo ( stm as M { variable , variableNames , ... } , v , suffix ) = let 

 val nv = ! variable 

 val () = variable := nv + 1 

 val nm = if suffix = "" then variableName ( stm , v ) else variableName ( stm , v ) ^ "_" ^ suffix 

 val nv = V nv 

 val () = variableNames := VariableDict.insert ( ! variableNames , nv , nm )  in nv end  

 fun variableRelated ( stm , v , suffix , info ) = let 

 val nv = variableRelatedNoInfo ( stm , v , suffix ) 

 val () = variableSetInfo ( stm , nv , info )  in nv end  

 fun variableDelete ( stm as M { variable , variableNames , variableInfo , ... } , v ) = let 

 val () = variableNames := VariableDict.remove ( ! variableNames , v ) 

 val () = variableInfo := VariableDict.remove ( ! variableInfo , v )  in ( ) end  

 fun nameMake ( M { name , names , rnames , ... } , str ) = case StringDict.lookup ( ! rnames , str ) of NONE => let 

 val nn = ! name 

 val () = name := nn + 1 

 val nn = N nn 

 val () = names := NameDict.insert ( ! names , nn , str ) 

 val () = rnames := StringDict.insert ( ! rnames , str , nn )  in nn end| SOME n => n  

 fun labelFresh ( M { label , ... } ) = let 

 val nl = ! label 

 val () = label := nl + 1  in L nl end  

 fun finishAux ( M { variable , name , label , variableNames , variableInfo , names , rnames } , info ) = let 

 fun doOne ( v , n , vns ) = if VariableDict.contains ( info , v ) then VariableDict.insert ( vns , v , n ) else vns  

 val vns = VariableDict.fold ( ! variableNames , VariableDict.empty , doOne )  in S { variableNames=vns , variableInfo=info , names=! names , rnames=! rnames , maxVariable=! variable , maxName=! name , maxLabel=! label } end  

 fun finish ( stm as M { variableInfo , ... } ) = finishAux ( stm , ! variableInfo )  

 fun finishMapFold ( stm as M { variableInfo , ... } , i , f ) = let 

 val ( info , i ) = VariableDict.mapFold ( ! variableInfo , i , f ) 

 val st = finishAux ( stm , info )  in ( st , i ) end  

 fun layoutVariable ( v , stm ) = Layout.str ( variableString ( stm , v ) )  

 fun layoutVariableEscaped ( v , stm ) = Layout.str ( variableStringEscaped ( stm , v ) )  

 fun layoutName ( n as N m , stm ) = let 

 val str = "_" ^ nameString ( stm , n ) handle _ => "" 

 val l = Layout.str ( "n" ^ ( Int.toString m ) ^ str )  in l end  

 fun layoutNameEscaped ( n as N m , stm ) = let 

 val str = "_" ^ nameStringEscaped ( stm , n ) handle _ => "" 

 val l = Layout.str ( "n" ^ ( Int.toString m ) ^ str )  in l end  

 fun new ord = let 

 val stm = M { variable=ref 0 , name=ref 0 , label=ref 0 , variableNames=ref VariableDict.empty , variableInfo=ref VariableDict.empty , names=ref NameDict.empty , rnames=ref StringDict.empty } 

 val _ = nameMake ( stm , ord )  in stm end   end structure SymbolInfo = struct 

 datatype 'a t = SiTable of 'a symbolTable | SiManager of 'a Manager.t 

 val variableExists = fn ( si , v ) => case si of SiTable st => variableExists ( st , v )| SiManager stm => Manager.variableExists ( stm , v ) 

 val variableLookup = fn ( si , v ) => case si of SiTable st => variableLookup ( st , v )| SiManager stm => Manager.variableLookup ( stm , v ) 

 val variableName = fn ( si , v ) => case si of SiTable st => variableName ( st , v )| SiManager stm => Manager.variableName ( stm , v ) 

 val variableNameEscaped = fn ( si , v ) => case si of SiTable st => variableNameEscaped ( st , v )| SiManager stm => Manager.variableNameEscaped ( stm , v ) 

 val variableInfo = fn ( si , v ) => case si of SiTable st => variableInfo ( st , v )| SiManager stm => Manager.variableInfo ( stm , v ) 

 val variableString = fn ( si , v ) => case si of SiTable st => variableString ( st , v )| SiManager stm => Manager.variableString ( stm , v ) 

 val variableStringEscaped = fn ( si , v ) => case si of SiTable st => variableStringEscaped ( st , v )| SiManager stm => Manager.variableStringEscaped ( stm , v ) 

 val nameExists = fn ( si , n ) => case si of SiTable st => nameExists ( st , n )| SiManager stm => Manager.nameExists ( stm , n ) 

 val nameString = fn ( si , n ) => case si of SiTable st => nameString ( st , n )| SiManager stm => Manager.nameString ( stm , n ) 

 val nameStringEscaped = fn ( si , n ) => case si of SiTable st => nameStringEscaped ( st , n )| SiManager stm => Manager.nameStringEscaped ( stm , n ) 

 val nameFromString = fn ( si , s ) => case si of SiTable st => nameFromString ( st , s )| SiManager stm => Manager.nameFromString ( stm , s ) 

 val layoutVariable = fn ( v , si ) => case si of SiTable st => layoutVariable ( v , st )| SiManager stm => Manager.layoutVariable ( v , stm ) 

 val layoutVariableEscaped = fn ( v , si ) => case si of SiTable st => layoutVariableEscaped ( v , st )| SiManager stm => Manager.layoutVariableEscaped ( v , stm ) 

 val layoutName = fn ( n , si ) => case si of SiTable st => layoutName ( n , st )| SiManager stm => Manager.layoutName ( n , stm ) 

 val layoutNameEscaped = fn ( n , si ) => case si of SiTable st => layoutNameEscaped ( n , st )| SiManager stm => Manager.layoutNameEscaped ( n , stm ) 

 val layoutLabel = fn ( l , _ ) => layoutLabel l  end  end ; 

