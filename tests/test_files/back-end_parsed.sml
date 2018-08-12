signature BACK_END = sig structure PilCompile : sig val pass : ( unit , unit ) Pass.t  end structure Link : sig val pass : ( unit , unit ) Pass.t  end  end structure BackEnd :> BACK_END = struct 

 val passname = "BackEnd" 

 val fail = fn ( f , m ) => Fail.fail ( passname , f , m ) structure Chat = ChatF ( struct 

 type env = Config.t 

 val extract = Utils.Function.id 

 val name = "BackEnd" 

 val indent = 0  end ) 

 fun useFutures ( config : Config.t ) = case Config.parStyle config of Config.PNone => false| Config.PAuto => true| Config.PAll => true| Config.PPar => true  

 fun singleThreaded ( config : Config.t ) = # singleThreaded ( Config.runtime config )  

 fun multiThreaded ( config : Config.t ) = useFutures config orelse not ( singleThreaded config )  

 fun synchronizeThunks ( config : Config.t ) = useFutures config orelse Config.synchronizeThunks config  

 fun ifDebug ( config , ad , a ) = if Config.pilDebug config then ad else a  

 val ( gcWriteBarriersF , gcWriteBarriers ) = Config.Feature.mk ( "Plsr:gc-write-barriers" , "generate GC write barriers for refs" ) 

 val ( gcAllBarriersF , gcAllBarriers ) = Config.Feature.mk ( "Plsr:all-barriers" , "generate non-optional write barriers" ) 

 val instrumentAllocationSites = MilToPil.instrumentAllocationSites 

 val backendYields = MilToPil.backendYields 

 val lightweightThunks = MilToPil.lightweightThunks 

 val interceptCuts = MilToPil.interceptCuts 

 val noGMP = MilToPil.noGMP 

 val zeroRefs = MilToPil.zeroRefs 

 val ( gcIndirectionsF , gcIndirections ) = Config.Feature.mk ( "Plsr:gc-indirections" , "GC cleans up thunk indirections" ) 

 val ( gmpUseGcMallocF , gmpUseGcMalloc ) = Config.Feature.mk ( "Plsr:gmp-use-gc-malloc" , "use gc-malloc for gmp integer wrappers" ) 

 val ( gmpUseMallocF , gmpUseMalloc ) = Config.Feature.mk ( "Plsr:gmp-use-malloc" , "use malloc for gmp integer wrappers" ) 

 val ( gmpUsePinningF , gmpUsePinning ) = Config.Feature.mk ( "Plsr:gmp-use-pinning" , "use pinning for gmp integer wrappers" ) 

 val ( gmpUsePCDeclF , gmpUsePCDecl ) = Config.Feature.mk ( "Plsr:gmp-use-pcdecl" , "use pcdecl for gmp integer wrappers" ) 

 val ( gmpUseForcedGcF , gmpUseForcedGc ) = Config.Feature.mk ( "Plsr:gmp-use-forced-gc" , "use gc forcing gmp integer wrappers" ) 

 val ( gmpUseGAllocateF , gmpUseGAllocate ) = Config.Feature.mk ( "Plsr:gmp-use-gallocate" , "use guaranteed allocation gmp integers" ) 

 val ( instrumentAllocationF , instrumentAllocation ) = Config.Feature.mk ( "Plsr:instrument-allocation" , "gather allocation statistics" ) 

 val ( instrumentGCsF , instrumentGCs ) = Config.Feature.mk ( "Plsr:instrument-gcs" , "gather allocation statistics per gc" ) 

 val ( instrumentVtbAllocationF , instrumentVtbAllocation ) = Config.Feature.mk ( "Plsr:instrument-vtb-alc" , "gather allocation statistics per vtable" ) 

 val ( p2cUseTryF , p2cUseTry ) = Config.Feature.mk ( "FLRC:p2c-use-try" , "Use try/except for continuations" ) 

 val ( vtableChangeF , vtableChange ) = Config.Feature.mk ( "Plsr:change-vtables" , "do vtable changing for immutability etc." ) 

 val ( noThunkSubsumptionF , noThunkSubsumption ) = Config.Feature.mk ( "Plsr:no-thunk-subsumption" , "don't use thunk/value subsumption" ) 

 val ( usePortableTaggedIntsF , usePortableTaggedInts ) = Config.Feature.mk ( "Plsr:tagged-ints-portable" , "tagged ints don't assume two's complement" ) 

 val ( assumeSmallIntsF , assumeSmallInts ) = Config.Feature.mk ( "Plsr:tagged-ints-assume-small" , "use 32 bit ints for tagged ints (unchecked)" ) 

 val ( noTaggedIntRecoverF , noTaggedIntRecover ) = Config.Feature.mk ( "Plsr:no-recover-tagged-ints" , "don't check output of AP arithmetic for small ints" ) 

 val ( disableTailCallF , disableTailCall ) = Config.Feature.mk ( "Plsr:disable-tail-call" , "implement tail calls as ordinary calls" ) 

 val opcStack = 2097152 

 val smallStack = 33554432 

 val largeStack = 536870912 

 fun defaultStackSize ( config : Config.t ) = ( case ( # stackMain ( Config.runtime config ) ) of SOME i => i| NONE => smallStack )  

 fun defaultStackStr ( config : Config.t ) = let 

 val i = defaultStackSize config 

 val s = Int.toString i  in s end  

 fun mainStackSize ( config : Config.t ) = ( case ( # stackMain ( Config.runtime config ) ) of SOME i => i| NONE => smallStack ) div ( 1024 * 1024 )  

 fun mainStackStr ( config : Config.t ) = let 

 val i = mainStackSize config 

 val s = Int.toString i  in s end  

 fun workerStackSize ( config : Config.t ) = ( case ( # stackWorker ( Config.runtime config ) ) of SOME i => i| NONE => opcStack * 2 ) div ( 1024 * 1024 )  

 fun workerStackStr ( config : Config.t ) = let 

 val i = workerStackSize config 

 val s = Int.toString i  in s end  

 fun sourceFile ( config , fname ) = fname ^ ".c"  

 fun objectFile ( config , fname ) = let 

 val linuxObjFile = fname ^ ".o" 

 val windowsObjFile = fname ^ ".obj"  in if Config.host config = Config.OsLinux then linuxObjFile else windowsObjFile end  

 fun exeFile ( config , fname ) = fname ^ ".exe"  

 fun compiler ( config ) = ( Pass.runWithSh , Path.fromString "pilicl" )  

 fun defines ( config : Config.t ) = let 

 val ws = case Config.targetWordSize config of Config.Ws32 => "P_WORD_SIZE=4"| Config.Ws64 => "P_WORD_SIZE=8" 

 val gc = case # style ( Config.gc config ) of Config.GcsNone => [ ]| Config.GcsConservative => [ "P_USE_CGC" ]| Config.GcsAccurate => ( case Config.agc config of Config.AgcGcMf => [ "P_AGC_LOCK_PARAM=0" , "P_USE_AGC=PlsrAKMf" ]| Config.AgcTgc => [ "P_AGC_LOCK_PARAM=1" , "P_USE_AGC=PlsrAKTgc" ]| Config.AgcCgc => [ "P_AGC_LOCK_PARAM=1" , "P_USE_AGC=PlsrAKCgc" ] ) @ ( if Config.agc config = Config.AgcTgc orelse Config.agc config = Config.AgcCgc then [ "P_USE_FAST_ALLOC" ] else [ ] ) @ ( if gcWriteBarriers config then [ "P_USE_GC_WRITE_BARRIERS" ] else [ ] ) @ ( if gcAllBarriers config then [ "P_ALL_BARRIERS" ] else [ ] ) 

 val pbase = case Config.output config of Config.OkPillar => [ "P_USE_PILLAR" , "WIN32" ]| Config.OkC => [ ] 

 val debug = ifDebug ( config , [ "GC_DEBUG" ] , [ "NDEBUG" ] ) 

 val futures = if useFutures config then [ "P_USE_PARALLEL_FUTURES" ] else [ ] 

 val synchThunks = if synchronizeThunks config then [ "PLSR_THUNK_SYNCHRONIZE" ] else [ ] 

 val singleThreaded = if singleThreaded config then [ "PLSR_SINGLE_THREADED" ] else [ ] 

 val thunks = ( if lightweightThunks config then [ "PLSR_LIGHTWEIGHT_THUNKS" ] else [ ] ) @ ( if interceptCuts config then [ "PLSR_THUNK_INTERCEPT_CUTS" ] else [ ] ) @ ( if noThunkSubsumption config then [ "PLSR_THUNK_NO_SUBSUMPTION" ] else [ ] ) @ ( if gcIndirections config then [ "PLSR_GC_INDIRECTIONS" ] else [ ] ) 

 val instr = List.concat [ if instrumentAllocation config then [ "PLSR_INSTRUMENT_ALLOCATION" ] else [ ] , if instrumentVtbAllocation config orelse instrumentAllocationSites config then [ "PLSR_INSTRUMENT_VTB_ALC" ] else [ ] , if instrumentGCs config then [ "PLSR_INSTRUMENT_GCS" ] else [ ] ] 

 val tailcall = if disableTailCall config then [ "PLSR_DISABLE_TAILCALL" ] else [ ] 

 val zeroRefDefine = if zeroRefs config then [ "PLSR_ZERO_REFS" ] else [ ] 

 val vtbChg = if vtableChange config then [ "P_DO_VTABLE_CHANGE" ] else [ ] 

 val va = let 

 val Config.VC { isa , ... } = Config.vectorConfig config  in case isa of Config.ViAVX => [ "P_USE_VI_AVX" ]| Config.ViMIC => [ "P_USE_VI_MIC" ]| Config.ViSSE _ => [ "P_USE_VI_SSE" ]| _ => [ ] end 

 val numericDefines = ( if PObjectModelLow.Rat.useUnsafeIntegers config then [ "P_PRAT_IS_SINTP" ] else [ ] ) @ ( if Globals.disableOptimizedRationals config then [ ] else [ "P_USE_TAGGED_RATIONALS" ] ) @ ( if Globals.disableOptimizedIntegers config then [ ] else [ "P_USE_TAGGED_INTEGERS" ] ) @ ( if usePortableTaggedInts config then [ "P_TAGGED_INT32_PORTABLE" ] else if assumeSmallInts config then [ "P_TAGGED_INT32_ASSUME_SMALL" ] else if MilToPil.assertSmallInts config then [ "P_TAGGED_INT32_ASSERT_SMALL" ] else [ ] ) @ ( if noGMP config then [ "PLSR_NO_GMP_INTEGERS" ] else [ ] ) @ ( if noTaggedIntRecover config then [ ] else [ "PLSR_TAGGED_INTEGER_RECOVER" ] ) @ ( if gmpUseMalloc config then [ "PLSR_GMP_USE_MALLOC" ] else if gmpUsePinning config then [ "PLSR_GMP_USE_PINNING" ] else if gmpUsePCDecl config then [ "PLSR_GMP_USE_PCDECL" ] else if gmpUseGcMalloc config then [ "PLSR_GMP_USE_GCMALLOC" ] else if gmpUseForcedGc config then [ "PLSR_GMP_USE_FORCE_GC" ] else if gmpUseGAllocate config then [ "PLSR_GMP_USE_GALLOCATE" ] else [ "PLSR_GMP_USE_DEFAULT" ] ) 

 val backend = [ "PPILER_BACKEND_IPC" ] 

 val stackSize = [ "PLSR_STACK_SIZE_WORKER=" ^ workerStackStr config , "PLSR_STACK_SIZE_MAIN=" ^ mainStackStr config ] 

 val ds = List.concat [ [ ws ] , gc , futures , singleThreaded , synchThunks , stackSize , thunks , debug , pbase , instr , tailcall , zeroRefDefine , vtbChg , va , numericDefines , backend ] 

 val flags = List.map ( ds , fn s => "-D" ^ s )  in flags end  structure CcOptions = struct 

 fun start ( config ) = [ "-p2c" ]  

 fun out ( config ) = [ "-c" ]  

 fun obj ( config , fname ) = let 

 val linuxStyle = "-o" ^ fname 

 val windowsStyle = "-Fo" ^ fname  in if Config.host config = Config.OsLinux then [ linuxStyle ] else [ windowsStyle ] end  

 fun debug ( config ) = let 

 val iccSymbolStr = ( if Config.host config = Config.OsLinux then "-g" else "-Zi" )  in ifDebug ( config , [ iccSymbolStr , "-debug" ] , [ iccSymbolStr ] ) end  

 fun arch ( config ) = if Config.host config = Config.OsLinux then [ "-xAVX" ] else [ "-QxAVX" ]  

 fun opt ( config ) = let 

 val level = Config.pilOpt config 

 val iccIp = ( if Config.host config = Config.OsLinux then "-ip" else "-Qip" ) 

 val vecRep0 = ( if Config.host config = Config.OsLinux then "-vec-report0" else "-Qvec-report0" ) 

 val disableCpuDispatch = ( if Config.host config = Config.OsLinux then [ "-diag-disable" , "cpu-dispatch" ] else [ "-Qdiag-disable:cpu-dispatch" ] ) 

 val ps = let 

 val opts = ( case level of 0 => [ "-Od" ]| 1 => [ "-O1" ]| 2 => [ "-O2" ]| 3 => [ "-O3" , iccIp , vecRep0 ] @ disableCpuDispatch| _ => fail ( "picc" , "Bad opt level" ) )  in opts end  in ps end  

 fun float ( config ) = let 

 val sloppy = Config.sloppyFp config 

 val fastModel = ( if Config.host config = Config.OsLinux then [ "-fp-model" , "fast" ] else [ "-fp:fast" ] ) 

 val sourceModel = ( if Config.host config = Config.OsLinux then [ "-fp-model" , "source" ] else [ "-fp:source" ] ) 

 val ftzYes = ( if Config.host config = Config.OsLinux then "-ftz" else "-Qftz" ) 

 val ftzNo = ( if Config.host config = Config.OsLinux then "-no-ftz" else "-Qftz-" ) 

 val precDivYes = ( if Config.host config = Config.OsLinux then "-prec-div" else "-Qprec-div" ) 

 val precDivNo = ( if Config.host config = Config.OsLinux then "-no-prec-div" else "-Qprec-div-" ) 

 val precSqrtYes = ( if Config.host config = Config.OsLinux then "-prec-sqrt" else "-Qprec-sqrt" ) 

 val precSqrtNo = ( if Config.host config = Config.OsLinux then "-no-prec-sqrt" else "-Qprec-sqrt-" ) 

 val vecNo = ( if Config.host config = Config.OsLinux then "-no-vec" else "-Qvec-" ) 

 val os = if sloppy then fastModel @ [ ftzYes , precDivNo , precSqrtNo ] else sourceModel @ [ ftzNo , precDivYes , precSqrtYes , vecNo ]  in os end  

 fun warn ( config ) = [ "-W3" , "-Qwd 177" , "-Qwd 279" ]  

 fun lang ( config ) = let 

 val iclC99 = ( if Config.host config = Config.OsLinux then "-std=c99" else "-Qstd=c99" )  in [ "-TC" , iclC99 ] end  

 fun contImp ( config ) = if p2cUseTry config then [ ] else [ "-p2c-no-use-try" ]  

 fun runtime ( config ) = if backendYields config then [ ] else [ "-Qnoyield" ]  

 fun mt ( config ) = ( if Config.host config = Config.OsLinux then [ ] else [ ifDebug ( config , "-MTd" , "-MT" ) ] )   end 

 fun compile ( config : Config.t , fname ) = let 

 val fname = Config.pathToHostString ( config , fname ) 

 val inFile = sourceFile ( config , fname ) 

 val outFile = objectFile ( config , fname ) 

 val cc = compiler config 

 val options = [ CcOptions.start config , CcOptions.out config , CcOptions.debug config , CcOptions.arch config , CcOptions.opt config , CcOptions.float config , CcOptions.warn config , CcOptions.lang config , CcOptions.contImp config , CcOptions.runtime config , CcOptions.mt config ] 

 val options = List.concat options 

 val defs = defines config 

 val args = [ options , defs , [ inFile ] , CcOptions.obj ( config , outFile ) , Config.pilcStr config ] 

 val args = List.concat args 

 val cleanup = fn () => if Config.keep ( config , "pil" ) then ( ) else File.remove inFile  in ( cc , args , cleanup ) end  

 fun linker ( config ) = ( Pass.runWithSh , Path.fromString "pilink" )  structure LdOptions = struct 

 fun exe ( config , fname ) = [ "-out:" ^ fname ]  

 fun libPath ( config , dname ) = "/LIBPATH:" ^ dname  

 fun fixGCCLibName s = if ( not ( String.contains ( s , #"\\" ) orelse String.contains ( s , #"/" ) ) ) andalso String.hasPrefix ( s , { prefix="lib" } ) andalso String.hasSuffix ( s , { suffix=".a" } ) then "-l" ^ String.dropPrefix ( String.dropSuffix ( s , 2 ) , 3 ) else s  

 fun lib ( config , lname ) = lname  

 fun start ( config ) = [ "-p2c" ]  

 fun control ( config ) = [ "-nologo" , "-INCREMENTAL:NO" ]  

 fun debug ( config ) = [ "-debug" ]   end 

 fun gcLibraries ( config ) = let 

 val gcs = # style ( Config.gc config ) 

 val failPillar = fn () => fail ( "gcLibraries" , "Conservative GC not supported on Pillar" ) 

 val libs = ( case gcs of Config.GcsNone => [ ]| Config.GcsConservative => failPillar ( )| Config.GcsAccurate => [ "imagehlp.lib" ] )  in libs end  

 fun futureLibraries ( config ) = let 

 val nm = if synchronizeThunks config then "parallel" else "sequential" 

 val gcs = ( case # style ( Config.gc config ) of Config.GcsConservative => "bdw_"| _ => "" ) 

 val file = "ptkfutures_p2c_" ^ nm ^ ".obj"  in [ file ] end  

 fun runtimeLibraries ( config ) = let 

 val hrcGhcRtLib = case Config.targetWordSize config of Config.Ws32 => "libhrc_ghc_runtime32.a"| Config.Ws64 => "libhrc_ghc_runtime64.a"  in [ hrcGhcRtLib ] end  

 fun unmanagedLibraries ( config ) = let 

 val threads = [ "Ws2_32.lib" , ifDebug ( config , "pthreadVC2d.lib" , "pthreadVC2.lib" ) ] 

 val gmpLibs = if noGMP config then [ ] else [ "libgmp.a" , "libgcc.a" ]  in threads @ gmpLibs end  

 fun libraries ( config ) = let 

 val ( prtBegin , prtEnd ) = ( [ "pillar2c_crt_begin.obj" ] , [ "pillar2c_crt_end.obj" ] ) 

 val gcLibs = gcLibraries config 

 val futureLibs = futureLibraries config 

 val runtimeLibs = runtimeLibraries config 

 val unmanagedLibs = unmanagedLibraries config 

 val pre = prtBegin 

 val post = List.concat [ futureLibs , prtEnd , gcLibs , runtimeLibs , unmanagedLibs ]  in ( pre , post ) end  

 fun link ( config , fname ) = let 

 val fname = Config.pathToHostString ( config , fname ) 

 val inFile = objectFile ( config , fname ) 

 val outFile = exeFile ( config , fname ) 

 val ld = linker config 

 val options = List.concat [ LdOptions.control config , LdOptions.debug config ] 

 val ( preLibs , postLibs ) = libraries config 

 val preLibs = List.map ( preLibs , fn l => LdOptions.lib ( config , l ) ) 

 val postLibs = List.map ( postLibs , fn l => LdOptions.lib ( config , l ) ) 

 val extraPaths = List.map ( Config.linkDirectories config , fn p => LdOptions.libPath ( config , p ) ) 

 val extraLibs = List.map ( Config.linkLibraries config , fn l => LdOptions.lib ( config , l ) ) 

 val args = List.concat [ LdOptions.exe ( config , outFile ) , LdOptions.start config , options , preLibs , [ inFile ] , postLibs , extraPaths , extraLibs , Config.linkStr config ] 

 val cleanup = fn () => if Config.keep ( config , "obj" ) then ( ) else File.remove inFile  in ( ld , args , cleanup ) end  

 val compile = fn ( config : Config.t , fname ) => let 

 val ( ( run , cmd ) , args , cleanup ) = compile ( config , fname ) 

 val () = Exn.finally ( fn () => run ( config , Chat.log0 , cmd , args ) , cleanup )  in ( ) end 

 val link = fn ( config : Config.t , fname ) => let 

 val ( ( run , cmd ) , args , cleanup ) = link ( config , fname ) 

 val () = Exn.finally ( fn () => run ( config , Chat.log0 , cmd , args ) , cleanup )  in ( ) end structure PilCompile = struct 

 val description = { name="PilCompile" , description="Compile Pil" , inIr=Pass.unitHelpers , outIr=Pass.unitHelpers , mustBeAfter=[ ] , stats=[ ] } 

 val associates = { controls=[ ] , debugs=[ ] , features=[ assumeSmallIntsF , disableTailCallF , gcWriteBarriersF , gcAllBarriersF , gcIndirectionsF , gmpUseForcedGcF , gmpUseGAllocateF , gmpUseGcMallocF , gmpUseMallocF , gmpUsePCDeclF , gmpUsePinningF , instrumentAllocationF , instrumentGCsF , instrumentVtbAllocationF , noTaggedIntRecoverF , noThunkSubsumptionF , p2cUseTryF , vtableChangeF , usePortableTaggedIntsF ] , subPasses=[ ] } 

 fun pilCompile ( () , pd , basename ) = compile ( PassData.getConfig pd , basename )  

 val pass = Pass.mkFilePass ( description , associates , pilCompile )  end structure Link = struct 

 val description = { name="Link" , description="Link the executable" , inIr=Pass.unitHelpers , outIr=Pass.unitHelpers , mustBeAfter=[ ] , stats=[ ] } 

 val associates = { controls=[ ] , debugs=[ ] , features=[ ] , subPasses=[ ] } 

 fun link' ( () , pd , basename ) = link ( PassData.getConfig pd , basename )  

 val pass = Pass.mkFilePass ( description , associates , link' )  end  end ; 

