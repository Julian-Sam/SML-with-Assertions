signature CONFIG = sig datatype agcProg = AgcGcMf | AgcTgc | AgcCgc datatype gcStyle = GcsNone | GcsConservative | GcsAccurate type gcConfig = { registerVtables : bool , reportGlobals : bool , reportRoots : bool , rootsInGlobals : bool , style : gcStyle } datatype os = OsCygwin | OsLinux | OsMinGW datatype outputKind = OkC | OkPillar datatype parStyle = PNone | PAll | PAuto | PPar type passInfo = { enable : bool , showPost : bool , showPre : bool , showLineCount : bool , statPost : bool , statPre : bool } type runtimeConfig = { stackWorker : int option , stackMain : int option , singleThreaded : bool } datatype verbosity = VSilent | VQuiet | VInfo | VTop datatype wordSize = Ws32 | Ws64 datatype vectorISA = ViANY | ViAVX | ViMIC | ViEMU | ViSSE of int * int datatype vectorConfig = VC of { isa : vectorISA , instructions : { disabled : string List.t , emulated : string List.t , enabled : string List.t } , sizes : { disabled : string List.t , emulated : string List.t , enabled : string List.t } } datatype t = C of { agc : agcProg , control_ : string StringDict.t , debugLev : verbosity , debug_ : StringSet.t , feature_ : StringSet.t , gc : { registerVtables : bool , reportGlobals : bool , reportRoots : bool , rootsInGlobals : bool , style : gcStyle } , ghcOpt : string list , home : Path.t , host : os , flrcOpt : int , keep : StringSet.t , linkStr : string list , linkDirectories : string list , linkLibraries : string list , logLev : verbosity , output : outputKind , parStyle : parStyle , passes : { enable : bool , showPost : bool , showPre : bool , showLineCount : bool , statPost : bool , statPre : bool } StringDict.t , pilcStr : string list , pilDebug : bool , pilOpt : int , report : StringSet.t , runtime : runtimeConfig , sloppyFp : bool , stop : string , synchThunks : bool , targetWordSize : wordSize , timeExecution : string option , vectorConfig : vectorConfig , warnLev : verbosity } val agc : t -> agcProg val debug : bool val debugLevel : t * 'a -> int val gc : t -> { registerVtables : bool , reportGlobals : bool , reportRoots : bool , rootsInGlobals : bool , style : gcStyle } val ghcOpt : t -> string list val home : t -> Path.t val host : t -> os val flrcOpt : t -> int val keep : t * string -> bool val linkStr : t -> string list val linkDirectories : t -> string list val linkLibraries : t -> string list val logLevel : t * 'a -> int val output : t -> outputKind val parStyle : t -> parStyle val passEnabled : t * string -> bool val passGet : t * string -> { enable : bool , showPost : bool , showPre : bool , showLineCount : bool , statPost : bool , statPre : bool } val passIs : t * string -> bool val passShowPost : t * string -> bool val passShowPre : t * string -> bool val passShowLineCount : t * string -> bool val passStatPost : t * string -> bool val passStatPre : t * string -> bool val pathToHostString : t * Path.t -> string val pilDebug : t -> bool val pilOpt : t -> int val pilcStr : t -> string list val reportEnabled : t * string -> bool val runtime : t -> runtimeConfig val silent : t -> bool val sloppyFp : t -> bool val stop : t -> string val synchronizeThunks : t -> bool val targetWordSize : t -> wordSize val targetWordSize' : t -> IntArb.size val timeExecution : t -> string option val verbose : t -> bool val vectorConfig : t -> vectorConfig val warnLevel : t * 'a -> int structure Control : sig type control val add : ( { check : string -> bool , describe : unit -> Layout.t } StringDict.t * string StringDict.t ref ) * string * string -> bool val describeControl : ( { check : string -> bool , describe : unit -> Layout.t } StringDict.t * 'a ) * string -> Layout.t val finalise : 'a * 'b ref -> 'b val isControl : ( 'a StringDict.t * 'b ) * string -> bool val listControls : 'a StringDict.t * 'b -> string list val mk : string * ( unit -> Layout.t ) * ( string -> 'a option ) * ( t -> 'a ) -> control * ( t -> 'a ) val mks : control list -> { check : string -> bool , describe : unit -> Layout.t } StringDict.t * 'a StringDict.t ref  end structure Debug : sig type debug val add : ( 'a StringDict.t * StringSet.t ref ) * string -> bool val finalise : 'a * 'b ref -> 'b val mk : string * string -> debug * ( t -> bool ) val mks : debug list -> string StringDict.t * StringSet.t ref val usage : string StringDict.t * 'a -> string  end structure Feature : sig type feature val add : ( 'a StringDict.t * StringSet.t ref ) * string -> bool val finalise : 'a * 'b ref -> 'b val mk : string * string -> feature * ( t -> bool ) val mks : feature list -> string StringDict.t * StringSet.t ref val usage : string StringDict.t * 'a -> string  end  end structure Config :> CONFIG = struct 

 val debug : bool = true 

 datatype outputKind = OkC | OkPillar 

 datatype parStyle = PNone | PAll | PAuto | PPar 

 datatype gcStyle = GcsNone | GcsConservative | GcsAccurate 

 datatype os = OsCygwin | OsLinux | OsMinGW 

 datatype agcProg = AgcGcMf | AgcTgc | AgcCgc 

 type gcConfig = { registerVtables : bool , reportRoots : bool , rootsInGlobals : bool , reportGlobals : bool , style : gcStyle } 

 datatype verbosity = VSilent | VQuiet | VInfo | VTop 

 datatype wordSize = Ws32 | Ws64 

 datatype vectorISA = ViEMU | ViSSE of int * int | ViAVX | ViMIC | ViANY 

 datatype vectorConfig = VC of { isa : vectorISA , instructions : { disabled : string List.t , emulated : string List.t , enabled : string List.t } , sizes : { disabled : string List.t , emulated : string List.t , enabled : string List.t } } 

 type passInfo = { enable : bool , showPre : bool , statPre : bool , showPost : bool , statPost : bool , showLineCount : bool } 

 type runtimeConfig = { stackWorker : int option , stackMain : int option , singleThreaded : bool } 

 datatype t = C of { agc : agcProg , control_ : string StringDict.t , debug_ : StringSet.t , debugLev : verbosity , feature_ : StringSet.t , gc : gcConfig , ghcOpt : string list , home : Path.t , host : os , flrcOpt : int , keep : StringSet.t , linkStr : string list , linkDirectories : string list , linkLibraries : string list , logLev : verbosity , output : outputKind , parStyle : parStyle , passes : passInfo StringDict.t , pilcStr : string list , pilDebug : bool , pilOpt : int , report : StringSet.t , runtime : runtimeConfig , stop : string , sloppyFp : bool , synchThunks : bool , targetWordSize : wordSize , timeExecution : string option , vectorConfig : vectorConfig , warnLev : verbosity } 

 fun get ( C config , p ) = p config  

 fun agc c = get ( c , # agc )  

 fun gc c = get ( c , # gc )  

 fun ghcOpt c = get ( c , # ghcOpt )  

 fun home c = get ( c , # home )  

 fun host c = get ( c , # host )  

 fun flrcOpt c = get ( c , # flrcOpt )  

 fun linkStr c = get ( c , # linkStr )  

 fun linkDirectories c = get ( c , # linkDirectories )  

 fun linkLibraries c = get ( c , # linkLibraries )  

 fun output c = get ( c , # output )  

 fun parStyle c = get ( c , # parStyle )  

 fun pilcStr c = get ( c , # pilcStr )  

 fun pilDebug c = get ( c , # pilDebug )  

 fun pilOpt c = get ( c , # pilOpt )  

 fun runtime c = get ( c , # runtime )  

 fun stop c = get ( c , # stop )  

 fun sloppyFp c = get ( c , # sloppyFp )  

 fun synchronizeThunks c = get ( c , # synchThunks )  

 fun targetWordSize c = get ( c , # targetWordSize )  

 fun timeExecution c = get ( c , # timeExecution )  

 fun vectorConfig c = get ( c , # vectorConfig )  

 fun debugLevel ( C cfg , name ) = case # debugLev cfg of VSilent => ~1| VQuiet => 0| VInfo => 1| VTop => 2  

 fun keep ( C c , s ) = StringSet.member ( # keep c , s )  

 fun logLevel ( C cfg , name ) = case # logLev cfg of VSilent => ~1| VQuiet => 0| VInfo => 1| VTop => 10  

 fun verbose ( C cfg ) = ( # logLev cfg ) = VTop  

 fun silent ( C cfg ) = ( # logLev cfg ) = VSilent  

 fun passIs ( C cfg , name ) = Option.isSome ( StringDict.lookup ( # passes cfg , name ) )  

 fun passGet ( C cfg , name ) = case StringDict.lookup ( # passes cfg , name ) of SOME i => i| NONE => Fail.fail ( "Config" , "passGet" , "unknown pass " ^ name )  

 fun passEnabled ( cfg , name ) = # enable ( passGet ( cfg , name ) )  

 fun passShowPre ( cfg , name ) = # showPre ( passGet ( cfg , name ) )  

 fun passStatPre ( cfg , name ) = # statPre ( passGet ( cfg , name ) )  

 fun passShowPost ( cfg , name ) = # showPost ( passGet ( cfg , name ) )  

 fun passStatPost ( cfg , name ) = # statPost ( passGet ( cfg , name ) )  

 fun passShowLineCount ( cfg , name ) = # showLineCount ( passGet ( cfg , name ) )  

 fun pathToHostString ( cfg , path ) = ( case host cfg of OsCygwin => Path.toWindowsString path| OsLinux => Path.toUnixString path| OsMinGW => Path.toWindowsString path )  

 fun reportEnabled ( C cfg , name ) = StringSet.member ( # report cfg , name )  

 fun targetWordSize' config = case targetWordSize config of Ws32 => IntArb.S32| Ws64 => IntArb.S64  

 fun warnLevel ( C cfg , name ) = case # warnLev cfg of VSilent => ~1| VQuiet => 0| VInfo => 1| VTop => 2  structure Debug = struct 

 datatype debug = D of string * string 

 fun mk ( name , description ) = if debug then let 

 fun enabled c = StringSet.member ( get ( c , # debug_ ) , name )  

 val d = D ( name , description )  in ( d , enabled ) end else let 

 fun enabled c = false  

 val d = D ( name , description )  in ( d , enabled ) end  

 fun mks l = if debug then let 

 fun doOne ( D ( n , d ) ) = ( n , d )  

 val debugs = StringDict.fromList ( List.map ( l , doOne ) )  in ( debugs , ref StringSet.empty ) end else let 

 val debugs = StringDict.fromList [ ]  in ( debugs , ref StringSet.empty ) end  

 fun add ( ( debugs , ds ) , d ) = if debug then if StringDict.contains ( debugs , d ) then let 

 val () = ds := StringSet.insert ( ! ds , d )  in true end else false else false  

 fun usage ( debugs , _ ) = if debug then let 

 val l = StringDict.toList debugs 

 fun doOne ( n , d ) = "  " ^ n ^ ": " ^ d ^ "\n"  

 val ss = List.map ( l , doOne ) 

 val s = String.concat ss  in s end else "Debug flags not available in release mode"  

 fun finalise ( _ , ds ) = ! ds   end structure Feature = struct 

 datatype feature = F of string * string 

 fun mk ( name , description ) = let 

 fun enabled c = StringSet.member ( get ( c , # feature_ ) , name )  

 val f = F ( name , description )  in ( f , enabled ) end  

 fun mks l = let 

 fun doOne ( F ( n , d ) ) = ( n , d )  

 val features = StringDict.fromList ( List.map ( l , doOne ) )  in ( features , ref StringSet.empty ) end  

 fun add ( ( features , fs ) , f ) = if StringDict.contains ( features , f ) then let 

 val () = fs := StringSet.insert ( ! fs , f )  in true end else false  

 fun usage ( features , _ ) = let 

 val l = StringDict.toList features 

 fun doOne ( n , d ) = "  " ^ n ^ ": " ^ d ^ "\n"  

 val ss = List.map ( l , doOne ) 

 val s = String.concat ss  in s end  

 fun finalise ( _ , fs ) = ! fs   end structure Control = struct 

 datatype control = C of { name : string , describe : unit -> Layout.t , check : string -> bool } 

 fun mk ( n , d , parse , dft ) = let 

 fun get_ c = case StringDict.lookup ( get ( c , # control_ ) , n ) of NONE => dft c| SOME x => Option.valOf ( parse x )  

 fun check s = Option.isSome ( parse s )  

 val c = C { name=n , describe=d , check=check }  in ( c , get_ ) end  

 type controls = { describe : unit -> Layout.t , check : string -> bool } StringDict.t 

 fun mks l = let 

 fun doOne ( C { name , describe , check } ) = ( name , { describe=describe , check=check } )  

 val controls = StringDict.fromList ( List.map ( l , doOne ) )  in ( controls : controls , ref StringDict.empty ) end  

 fun isControl ( ( controls , _ ) , c ) = StringDict.contains ( controls , c )  

 fun add ( ( controls : controls , cs ) , c , s ) = if # check ( Option.valOf ( StringDict.lookup ( controls , c ) ) ) s then let 

 val () = cs := StringDict.insert ( ! cs , c , s )  in true end else false  

 fun describeControl ( ( controls : controls , _ ) , c ) = # describe ( Option.valOf ( StringDict.lookup ( controls , c ) ) ) ( )  

 fun listControls ( controls , _ ) = List.map ( StringDict.toList controls , # 1 )  

 fun finalise ( _ , cs ) = ! cs   end  end ; 

