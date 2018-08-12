signature PATH = sig type t datatype volume = Root | Drive of char val fromString : string -> t val fromCygwinString : string -> t val fromMinGWString : string -> t val fromUnixString : string -> t val fromWindowsString : string -> t val toCygwinString : t -> string val toWindowsString : t -> string val toUnixString : t -> string val toMinGWString : t -> string val layout : t -> Layout.t val cons : string * t -> t val snoc : t * string -> t val append : t * t -> t val setVolume : t * volume -> t val removeVolume : t -> t val dropLast : t -> t  end structure Path :> PATH = struct 

 datatype volume = Root | Drive of char 

 datatype t = PRel of string list | PAbs of volume * ( string list ) structure SP = StringParser 

 val || = SP.|| 

 val && = SP.&&  infix || && 

 val isChar = fn c => SP.satisfy ( fn c' => c = c' ) 

 val alpha = SP.satisfy Char.isAlpha 

 val ::: = fn ( a , b ) => SP.map ( a && b , fn ( a , b ) => a :: b ) 

 val -&& = fn ( a , b ) => SP.map ( a && b , fn ( a , b ) => b ) 

 val &&- = fn ( a , b ) => SP.map ( a && b , fn ( a , b ) => a )  infixr 5 :::  infix -&& &&- 

 val parseRelative : ( string SP.t * char SP.t ) -> string List.t SP.t = fn ( filename , sep ) => let 

 val eof = SP.map ( SP.atEnd , fn () => [ ] ) 

 val rec relative = fn () => ( filename ::: ( sep -&& ( SP.$ relative ) ) ) || ( filename ::: ( sep -&& eof ) ) || ( filename ::: eof ) 

 val p = SP.map ( SP.$ relative , List.rev )  in p end 

 val parseWindowsStyle : t StringParser.t = let 

 val invalidFileChar = fn c => ( Char.ord c < 32 ) orelse ( c = #"<" ) orelse ( c = #">" ) orelse ( c = #":" ) orelse ( c = #"\"" ) orelse ( c = #"/" ) orelse ( c = #"\\" ) orelse ( c = #"|" ) orelse ( c = #"?" ) orelse ( c = #"*" ) 

 val validFileChar = not o invalidFileChar 

 val filename = SP.map ( SP.oneOrMore ( SP.satisfy validFileChar ) , String.implode ) 

 val sep = ( isChar #"\\" ) || ( isChar #"/" ) 

 val relative = parseRelative ( filename , sep ) 

 val volume = alpha &&- ( isChar #":" ) &&- sep 

 val absolute = volume && relative 

 val rooted = sep -&& relative 

 val p = ( SP.map ( absolute , fn ( drive , s ) => PAbs ( Drive drive , s ) ) ) || ( SP.map ( rooted , fn s => PAbs ( Root , s ) ) ) || ( SP.map ( relative , fn s => PRel s ) )  in p end 

 val parseUnixStyle : t StringParser.t = let 

 val invalidFileChar = fn c => ( Char.ord c = 0 ) orelse ( c = #"/" ) orelse ( c = #"\\" ) 

 val validFileChar = not o invalidFileChar 

 val filename = SP.map ( SP.oneOrMore ( SP.satisfy validFileChar ) , String.implode ) 

 val sep = ( isChar #"/" ) || ( isChar #"\\" ) 

 val relative = parseRelative ( filename , sep ) 

 val absolute = sep -&& relative 

 val p = ( SP.map ( absolute , fn s => PAbs ( Root , s ) ) ) || ( SP.map ( relative , fn s => PRel s ) )  in p end 

 val parsePath = parseWindowsStyle || parseUnixStyle 

 val convertCygwinVolume = fn p => ( case p of PAbs ( Root , arcs ) => ( case List.rev arcs of ( "cygdrive" :: vol :: arcs ) => if String.length vol = 1 then PAbs ( Drive ( String.sub ( vol , 0 ) ) , List.rev arcs ) else p| _ => p )| _ => p ) 

 val fromString : string -> t = fn s => let 

 val path = ( case SP.parse ( parsePath , ( s , 0 ) ) of SP.Success ( _ , p ) => convertCygwinVolume p| _ => Fail.fail ( "Path" , "fromString" , "Can't parse path: " ^ s ) )  in path end 

 val fromCygwinString : string -> t = fn s => let 

 val path = ( case SP.parse ( parseUnixStyle , ( s , 0 ) ) of SP.Success ( _ , p ) => convertCygwinVolume p| _ => Fail.fail ( "Path" , "fromCygwinString" , "Can't parse path: " ^ s ) )  in path end 

 val fromMinGWString : string -> t = fn s => let 

 val path = ( case SP.parse ( parseUnixStyle , ( s , 0 ) ) of SP.Success ( _ , p ) => p| _ => Fail.fail ( "Path" , "fromMinGWString" , "Can't parse path: " ^ s ) )  in path end 

 val fromUnixString : string -> t = fn s => let 

 val path = ( case SP.parse ( parseUnixStyle , ( s , 0 ) ) of SP.Success ( _ , p ) => p| _ => Fail.fail ( "Path" , "fromUnixString" , "Can't parse path: " ^ s ) )  in path end 

 val fromWindowsString : string -> t = fn s => let 

 val path = ( case SP.parse ( parseWindowsStyle , ( s , 0 ) ) of SP.Success ( _ , p ) => p| _ => Fail.fail ( "Path" , "fromWindowsString" , "Can't parse path: " ^ s ) )  in path end 

 val collapseArcs : string list * string -> string = String.concat o List.rev o List.separate 

 val toCygwinString : t -> string = ( fn path => let 

 val collapse = fn arcs => collapseArcs ( arcs , "/" ) 

 val s = ( case path of PRel arcs => collapse arcs| PAbs ( Root , arcs ) => "/" ^ collapse arcs| PAbs ( Drive c , arcs ) => "/cygdrive/" ^ ( Char.toString c ) ^ "/" ^ collapse arcs )  in s end ) 

 val toWindowsString : t -> string = ( fn path => let 

 val collapse = fn arcs => collapseArcs ( arcs , "\\" ) 

 val s = ( case path of PRel arcs => collapse arcs| PAbs ( Root , arcs ) => Fail.fail ( "Path" , "toWindowsString" , "Can't use root volume in windows" )| PAbs ( Drive c , arcs ) => ( Char.toString c ) ^ ":\\" ^ collapse arcs )  in s end ) 

 val toUnixString : t -> string = ( fn path => let 

 val collapse = fn arcs => collapseArcs ( arcs , "/" ) 

 val s = ( case path of PRel arcs => collapse arcs| PAbs ( Root , arcs ) => "/" ^ collapse arcs| PAbs ( Drive c , arcs ) => Fail.fail ( "Path" , "toUnixString" , "Can't use Drive in windows" ) )  in s end ) 

 val toMinGWString : t -> string = ( fn path => let 

 val collapse = fn arcs => collapseArcs ( arcs , "/" ) 

 val s = ( case path of PRel arcs => collapse arcs| PAbs ( Root , arcs ) => "/" ^ collapse arcs| PAbs ( Drive c , arcs ) => "/" ^ ( Char.toString c ) ^ "/" ^ collapse arcs )  in s end ) 

 val layout : t -> Layout.t = ( fn path => let 

 val lArcs = List.layout Layout.str 

 val l = ( case path of PRel arcs => Layout.seq [ Layout.str "Relative: " , lArcs arcs ]| PAbs ( Root , arcs ) => Layout.seq [ Layout.str "Rooted: " , lArcs arcs ]| PAbs ( Drive c , arcs ) => let 

 val c = Char.toString c  in Layout.seq [ Layout.str "Drive " , Layout.str c , Layout.str ": " , lArcs arcs ] end )  in l end ) 

 val cons : string * t -> t = fn ( s , path ) => ( case path of PRel arcs => PRel ( arcs @ [ s ] )| PAbs _ => Fail.fail ( "Path" , "cons" , "Can't cons to absolute path" ) ) 

 val snoc : t * string -> t = fn ( path , s ) => ( case path of PRel arcs => PRel ( s :: arcs )| PAbs ( vol , arcs ) => PAbs ( vol , s :: arcs ) ) 

 val append : t * t -> t = fn ( path1 , path2 ) => ( case ( path1 , path2 ) of ( PRel arcs1 , PRel arcs2 ) => PRel ( arcs2 @ arcs1 )| ( PAbs ( vol , arcs1 ) , PRel arcs2 ) => PAbs ( vol , arcs2 @ arcs1 )| _ => Fail.fail ( "Path" , "append" , "Can't append absolute path" ) ) 

 val setVolume : t * volume -> t = fn ( path , vol ) => ( case path of PRel arcs => PAbs ( vol , arcs )| PAbs ( _ , arcs ) => PAbs ( vol , arcs ) ) 

 val removeVolume : t -> t = fn path => ( case path of PRel arcs => path| PAbs ( _ , arcs ) => PRel arcs ) 

 val dropLast : t -> t = fn path => ( case path of PRel ( _ :: arcs ) => PRel arcs| PAbs ( v , _ :: arcs ) => PAbs ( v , arcs )| _ => Fail.fail ( "Path" , "dropLast" , "cannot drop empty path" ) )  end 

