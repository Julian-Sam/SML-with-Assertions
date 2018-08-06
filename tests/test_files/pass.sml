(* The Haskell Research Compiler *)
(*
 * Redistribution and use in source and binary forms, with or without modification, are permitted 
 * provided that the following conditions are met:
 * 1.   Redistributions of source code must retain the above copyright notice, this list of 
 * conditions and the following disclaimer.
 * 2.   Redistributions in binary form must reproduce the above copyright notice, this list of
 * conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


(* Pass control file *)

(* Provides for describing passes and for performing them *)

signature PASS_DATA =
sig

  type t

  val getConfig : t -> Config.t
  val getStats : t -> Stats.t
  val getLevel : t -> int
  val click : t * string -> unit
  val clickN : t * string * int -> unit
  val clicker : {stats : (string * string) List.t, passname : string, name : string, desc : string} 
                -> {stats : (string * string) List.t, click : t -> unit}
  val mk : Config.t * (string * string) list -> t
  val mk' : Config.t * Stats.t * int -> t
  val push : t -> t
  val report : t * string -> unit
  (* Make a feature, enabled by command line *)
  val mkFeature : string * string -> (Config.Feature.feature * (t -> bool))
  (* Make a feature which is enabled by command line, or if log level is greater than threshold *)
  val mkLogFeature : string * string * string * int -> (Config.Feature.feature * (t -> bool))
  (* Make a debug, enabled by command line *)
  val mkDebug : string * string -> (Config.Debug.debug * (t -> bool))
  (* mkLevelDebug (passname, tag, description, level, debugPass)
   * Make a debug which is enabled if:
   *  it is set on the command line
   *  or if the debug level is greater than level and debugPass returns true
   *)
  val mkLevelDebug : string * string * string * int * (t -> bool) -> (Config.Debug.debug * (t -> bool)) 

  val phase : {measure : 'state * t  * string * 'object -> unit,
               print   : 'state * t * string * 'object -> unit,
               log     : t * string -> unit} 
              -> {name       : string,
                  doIt       : 'state * t * 'object -> 'object,
                  measureItP : t -> bool,
                  printItP   : t -> bool,
                  skipItP    : t -> bool,
                  statItP    : t -> bool} 
              -> 'state * t * 'object -> 'object

end;

structure PassData :> PASS_DATA =
struct

  datatype t = PD of {config : Config.t, stats : Stats.t, level : int}

  fun getConfig (PD {config, ...}) = config
  fun getStats (PD {stats, ...}) = stats
  fun getLevel (PD {level, ...}) = level

  fun click (d, s) = Stats.incStat (getStats d, s)

  fun clickN (d, s, n) = Stats.addToStat (getStats d, s, n)

  fun clicker {stats, passname, name, desc} =
      let
        val globalNm = passname ^ ":" ^ name
        val stats = 
            if List.exists (stats, fn (nm, _) => nm = globalNm) then
              Fail.fail ("PassData", "clicker", "Duplicate stat")
            else
              (globalNm, desc) :: stats
        val click = fn pd => click (pd, globalNm)
      in {stats = stats, click = click}
      end

  fun mk' (config, stats, level) = PD {config = config, stats = stats, level = level}

  fun mk (config, stats) = mk' (config, Stats.fromList stats, 0)

  fun push (PD {config, stats, level}) =
      PD {config = config, stats = Stats.push stats, level = level + 1}

  fun report (pd, passname) = 
      if Config.reportEnabled(getConfig pd, passname) then
        Stats.report (getStats pd)
      else
        ()

  val mkLogFeature : string * string * string * int -> (Config.Feature.feature * (t -> bool)) = 
   fn (passname, tag, description, level) =>
      let
        val (featureD, feature) = 
            Config.Feature.mk (tag, description)
        val feature = 
         fn d => 
            let
              val config = getConfig d
            in feature config orelse 
               (Config.logLevel (config, passname) >= level)
            end
      in (featureD, feature)
      end

  val mkFeature : string * string -> (Config.Feature.feature * (t -> bool)) = 
   fn (tag, description) =>
      let
        val (featureD, feature) = 
            Config.Feature.mk (tag, description)
        val feature = 
         fn d => feature (getConfig d)
      in (featureD, feature)
      end

  val mkDebug : string * string -> (Config.Debug.debug * (t -> bool)) = 
   fn (tag, description) =>
      let
        val (debugD, debug) = 
            Config.Debug.mk (tag, description)
        val debug = 
         fn d => 
            let
              val config = getConfig d
            in debug config 
            end
      in (debugD, debug)
      end

  val mkLevelDebug : string * string * string * int * (t -> bool) -> (Config.Debug.debug * (t -> bool)) = 
   fn (passname, tag, description, level, debugPass) =>
      let
        val (debugD, debug) = mkDebug (tag, description)
        val debug = 
         fn d => 
            let
              val config = getConfig d
            in debug d orelse 
               (debugPass d andalso Config.debugLevel (config, passname) >= level)
            end
      in (debugD, debug)
      end

  val phase =
   fn {measure, print, log} => 
      fn {name, doIt, measureItP, skipItP, printItP, statItP} => 
         fn (state, pd, object) => 
            if skipItP pd then
              let
                val () = log (pd, "Skipping "^name)
              in object
              end
            else
              let
                val pd = push pd
                val () = log (pd, "Doing "^name)
                val s = Time.now ()
                val object = doIt (state, pd, object)
                val e = Time.toString (Time.- (Time.now (), s))
                val () = log (pd, "Done with "^name^" in "^e^"s")
                val () = if statItP pd then Stats.report (getStats pd) else ()
                val () = if measureItP pd then measure (state, pd, name, object) else ()
                val () = if printItP pd then print (state, pd, name, object) else ()
              in object
              end

end;

signature PASS =
sig
  type ('a, 'b) t
  type ('a, 'b) processor
  type 'a irHelpers = {
       printer : 'a * Config.t -> Layout.t,
       stater  : 'a * Config.t -> Layout.t
  }
  type ('a, 'b) description = {
       name        : string,
       description : string,
       inIr        : 'a irHelpers,
       outIr       : 'b irHelpers,
       mustBeAfter : ('a, 'b) t list,
       stats       : (string * string) list
  }
  type ('a, 'b) associates = {
       controls  : Config.Control.control list,
       debugs    : Config.Debug.debug list,
       features  : Config.Feature.feature list,
       subPasses : ('a, 'b) t list
  }
  val unitHelpers : unit irHelpers
  val mkCompulsoryPass : ('a, 'b) description
                         * ('a, 'b) associates
                         * ('a * PassData.t -> 'b) 
                         -> ('a, 'b) t
  val mkOptPass : ('a, 'a) description
                  * ('a, 'a) associates
                  * ('a * PassData.t -> 'a)
                  -> ('a, 'a) t
  val mkOptFullPass : ('a, 'a) description
                      * ('a, 'a) associates
                      * ('a * PassData.t * Path.t -> 'a)
                      -> ('a, 'a) t
  val mkFilePass : ('a, 'b) description
                   * ('a, 'b) associates
                   * ('a * PassData.t * Path.t -> 'b) 
                   -> ('a, 'b) t
  val getDescription : ('a, 'b) t -> ('a, 'b) description
  val getName : ('a, 'b) t -> string
  val getDescriptionS : ('a, 'b) t -> string
  val getMustBeAfter : ('a, 'b) t -> ('a, 'b) t list
  val getStats : ('a, 'b) t -> (string * string) list
  val getAssociates : ('a, 'b) t -> ('a, 'b) associates
  val isOptional : ('a, 'b) t -> bool
  (* check (ps1, ps2):
   *   ps1 is a list of possible passes
   *   ps2 is a list of passes to run
   *   check that each compulsory pass is in ps2
   *   check that each pass in ps2 comes after the passes it is supposed to
   *)
  val check : ('a, 'b) t list * ('a, 'b) t list -> bool
  type driverInfo =
       Config.Control.control list * Config.Debug.debug list * Config.Feature.feature list *
       (string * string) list * {description : string, optional : bool} StringDict.t
  val addPassDriverInfo : ('a, 'b) t * driverInfo -> driverInfo
  val doPass : ('a, 'b) t -> ('a, 'b) processor
  val doPassWrap : ('a, 'b) t * ('c -> 'a) * ('b -> 'd) -> ('c, 'd) processor
  val doSubPass : ('a, 'b) t -> ('a, 'b) processor
  exception Done
  val stopAt : string -> ('a, 'a) processor
  val extractConfig : ('a, 'a * Config.t) processor 
  val >> : ('a, 'b) processor * ('b, 'c) processor -> ('a, 'c) processor
  val >>> : ('a, 'b * Config.t) processor * ('b, 'c) processor -> ('a, 'c) processor
  val first : ('a, 'b) processor -> ('a * 'c, 'b * 'c) processor
  val ifC : (Config.t -> bool) * ('a, 'b) processor * ('a, 'b) processor -> ('a, 'b) processor
  val apply : ('a, 'b) processor -> PassData.t * Path.t * 'a -> 'b
  val startFile : Config.t * string -> unit
  val endFile : Config.t * string -> unit
  val doPassPart : Config.t * string * (unit -> 'a) -> 'a
  val runCmd : string * string list * string list * bool -> string
  val run :
      Config.t * (Config.t * string -> unit) * Path.t * string list -> unit
  val runWithSh :
      Config.t * (Config.t * string -> unit) * Path.t * string list -> unit
end;

structure Pass :> PASS =
struct

  type 'a irHelpers = {
       printer : 'a * Config.t -> Layout.t,
       stater  : 'a * Config.t -> Layout.t
  }

  datatype ('a, 'b) t = P of {
           description : ('a, 'b) description,
           associates  : ('a, 'b) associates,
           optional    : bool,
           f           : PassData.t * Path.t * 'a -> 'b
  }
  withtype ('a, 'b) associates = {
       controls  : Config.Control.control list,
       debugs    : Config.Debug.debug list,
       features  : Config.Feature.feature list,
       subPasses : ('a, 'b) t list
  }
  and ('a, 'b) description = {
      name        : string,
      description : string,
      inIr        : 'a irHelpers,
      outIr       : 'b irHelpers,
      mustBeAfter : ('a, 'b) t list,
      stats       : (string * string) list
  }

  datatype ('a, 'b) processor = T of PassData.t * Path.t * 'a -> 'b

  val unitHelpers = {
      printer = fn _ => Layout.str "Success!",
      stater  = fn _ => Layout.empty
  }

  fun getDescription (P {description, ...}) = description
  fun getName  p = #name (getDescription p)
  fun getDescriptionS p = #description (getDescription p)
  fun getInIr p = #inIr (getDescription p)
  fun getOutIr p = #outIr (getDescription p)
  fun getMustBeAfter p = #mustBeAfter (getDescription p)
  fun getStats p = #stats (getDescription p)
  fun getAssociates (P {associates, ...}) = associates
  fun isOptional (P {optional, ...}) = optional
  fun getF (P {f, ...}) = f

  structure Chat = ChatF (type env = Config.t
                          fun extract x = x
                          val name = "Toplevel"
                          val indent = 0)

  fun bold s = 
      let
        val s = "========== " ^ s
        val s = StringCvt.padRight #"=" 60 (s ^ " ")
      in s
      end

  fun startFile (config, fname) =
      Chat.log0 (config, bold ("Starting File " ^ fname))

  fun endFile (config, fname) =
      Chat.log0 (config, bold ("Finished File " ^ fname))

  fun doLayout (config, msg, l) =
      let
        val () = Chat.log0 (config, bold msg)
        val () = LayoutUtils.printLayout l
      in ()
      end

  fun doLineCount (config, msg, l) =
      let
        val () = Chat.log0 (config, bold msg)
        val () = LayoutUtils.printLayout (Layout.seq [
                   Layout.str "Total Line Count is ",
                   Int.layout (LayoutUtils.countLines l)])
      in ()
      end

  fun doPass p =
   fn (pd, basename, arg) =>
     let
       val config = PassData.getConfig pd
       val name = getName p
       val inIr = getInIr p
       val outIr = getOutIr p
       val doIt = getF p
       val () =
           if Config.passShowPre (config, name) then
             doLayout (config, "Before Pass " ^ name ^ " IR",
                       (#printer inIr) (arg, config))
           else ()
       val () =
           if Config.passStatPre (config, name) then
             doLayout (config, "Before Pass " ^ name ^ " IR Stats",
                       (#stater inIr) (arg, config))
           else ()
       val ll = if PassData.getLevel pd > 0 then 1 else 0
       val () = Chat.log (config, ll, bold ("Starting Pass " ^ name))
       val t1 = Time.now ()
       val output = doIt (pd, basename, arg)
       val t2 = Time.now ()
       val ts = Time.toString (Time.- (t2, t1))
       val msg = "Finished Pass " ^ name ^ " (" ^ ts ^"s)"
       val () = Chat.log (config, ll, bold msg)
       val () =
           if Config.passShowPost (config, name) then
             doLayout (config, "After Pass " ^ name ^ " IR",
                       (#printer outIr) (output, config))
           else ()
       val () = 
           if Config.passShowLineCount (config, name) then
             doLineCount (config, "After Pass " ^ name ^ " IR Line Count",
                       (#printer outIr) (output, config))
           else ()
       val () =
           if Config.passStatPost (config, name) then
             doLayout (config, "After Pass " ^ name ^ " IR Stats",
                       (#stater outIr) (output, config))
           else ()
     in output
     end

  fun skipPass (pd, name, skipIt, arg) = 
      let
        val () = Chat.log0 (PassData.getConfig pd,
                            bold ("Skipping Pass " ^ name))
        val res = skipIt (name, arg)
      in res
      end

  fun newPass (d, a, f, opt, skip) =
      let
        val p = P {description = d, associates = a, optional = opt, f = f}
        fun f (pd, basename, arg) =
            if Config.passEnabled (PassData.getConfig pd, #name d) then
              doPass p (pd, basename, arg)
            else
              skipPass (pd, #name d, skip, arg)
        val p = P {description = d, associates = a, optional = opt, f = f}
      in p
      end

  fun bad (n, _) =
      Fail.fail ("Pass", "bad", "Pass " ^ n ^ " cannot be disabled")

  fun skip (_, a) = a

  fun mkCompulsoryPass (d, a, f) =
      newPass (d, a, (fn (pd, _, a) => f (a, pd)), false, bad)

  fun mkOptPass (d, a, f) =
      newPass (d, a, (fn (pd, _, a) => f (a, pd)), true, skip)

  fun mkOptFullPass (d, a, f) =
      newPass (d, a, (fn (pd, b, a) => f (a, pd, b)), true, skip)

  fun mkFilePass (d, a, f) =
      newPass (d, a, (fn (pd, b, a) => f (a, pd, b)), false, bad)

  fun check (ps1, ps2) =
     let
       fun doOne (p, pns) = StringSet.insert (pns, getName p)
       val pns2 = List.fold (ps2, StringSet.empty, doOne)
       fun doOne p = isOptional p orelse StringSet.member (pns2, getName p)
       val allCompuls = List.forall (ps1, doOne)
       fun loop (ps, pns) =
           case ps
            of [] => true
             | p::ps =>
               let
                 val after = getMustBeAfter p
                 fun checkOne p = not (StringSet.member (pns, getName p))
               in
                 if List.forall (after, checkOne) then
                   loop (ps, StringSet.insert (pns, getName p))
                 else
                   false
               end
       val depsOk = loop (List.rev ps2, StringSet.empty)
     in allCompuls andalso depsOk
     end

  type driverInfo =
       Config.Control.control list * Config.Debug.debug list * Config.Feature.feature list *
       (string * string) list * {description : string, optional : bool} StringDict.t

  fun addPassDriverInfo (p, (cs, ds, fs, ss, pm)) =
      let
        val n = getName p
        val d = getDescriptionS p
        val opt = isOptional p
        val a = getAssociates p
        val cs = cs @ #controls a
        val ds = ds @ #debugs a
        val fs = fs @ #features a
        val ss = ss @ getStats p
        val pd = {description = d, optional = opt}
        val pm = StringDict.insert (pm, n, pd)
        val x = List.fold (#subPasses a, (cs, ds, fs, ss, pm), addPassDriverInfo)
      in x
      end

  fun doPassPart (config, part, f) =
      let
        val () = Chat.log2 (config, "Starting part " ^ part)
        val t1 = Time.now ()
        val output = f ()
        val t2 = Time.now ()
        val ts = Time.toString (Time.- (t2, t1))
        val msg = "Finished part " ^ part ^ " (" ^ ts ^"s)"
        val () = Chat.log2 (config, msg)
      in output
      end

  exception Done

  fun doPass p = T (getF p)
  fun doPassWrap (p, f, g) =
      let
        fun h (pd, bn, x) =
            let
              val y = f x
              val z = getF p (pd, bn, y)
              val w = g z
            in w
            end
      in T h
      end
  fun doSubPass p = T (getF p o (fn (pd, bn, x) => (PassData.push pd, bn, x)))

  fun stopAt sp =
      let
        fun f (pd, _, arg) = if sp = Config.stop (PassData.getConfig pd) then raise Done else arg
      in T f
      end

  val extractConfig = T (fn (pd, _, arg) => (arg, PassData.getConfig pd))

  fun >> (T f1, T f2) =
      let
        fun f (pd, base, arg) = f2 (pd, base, f1 (pd, base, arg))
      in T f
      end

  fun >>> (T f1, T f2) =
      let
        fun f (pd, base, arg) = 
            let
              val (r, config) = f1 (pd, base, arg)
              val pd = PassData.mk' (config, PassData.getStats pd, PassData.getLevel pd)
            in
              f2 (pd, base, r)
            end
      in T f
      end

  fun first (T f1) =
      let
        fun f (pd, base, (arg, extra)) = let val r = f1 (pd, base, arg) in (r, extra) end
      in T f
      end

  fun ifC (f, T f1, T f2) =
      let
        val f = fn (pd, base, arg) => if f (PassData.getConfig pd) then f1 (pd, base, arg) else f2 (pd, base, arg)
      in T f
      end

  fun apply (T f1) = f1

  fun lookupCmdInEnv (cmd, morePaths) =
      case Process.getEnv "PATH"
        of NONE => Fail.fail ("pass", "run", "missing PATH variable in environment")
         | SOME paths => 
          let
            val (d, s, ext) = case MLton.Platform.OS.host
                            of MLton.Platform.OS.MinGW => (#";", "\\", ".exe")
                             | _ => (#":", "/", "")
            val paths = morePaths @ String.split (paths, d)
            (* try to be smart about file extensions of executables *)
            val basename = #file (OS.Path.splitDirFile cmd)
            val cmdExe = if String.contains (basename, #".") then cmd else cmd ^ ext
            fun find (d :: dirs) = 
                let
                  val p = d ^ s ^ cmdExe
                in
                  if File.doesExist p andalso File.canRun p
                    then p
                    else find dirs
                end
              | find [] = Fail.fail ("pass", "run", "command " ^ cmd ^
                                     " cannot be found in PATH")
          in
            if File.doesExist cmdExe andalso File.canRun cmdExe
              then cmdExe
              else find paths
          end

  (*
   * When silent, it returns the output from running the command;
   * otherwise, prints output and return empty string.
   *)
  fun runCmd (cmd, args, morePaths, silent) = 
      let 
        val cmdPath = lookupCmdInEnv (cmd, morePaths)
        val p = MLton.Process.create 
                   { args = args
                   , env  = NONE
                   , path = cmdPath
                   , stderr = MLton.Process.Param.self
                   , stdin  = MLton.Process.Param.null
                   , stdout = MLton.Process.Param.pipe
                   }
        fun echo h = case TextIO.inputLine h 
                       of NONE => ""
                        | SOME s => (print s; echo h)
        val wrap = if silent then TextIO.inputAll else echo
      in
        wrap (MLton.Process.Child.textIn (MLton.Process.getStdout p))
          before 
            (case MLton.Process.reap p 
              of Posix.Process.W_EXITED => ()
               | Posix.Process.W_EXITSTATUS s => Fail.fail ("pass", "run", "command exit status " ^ Word8.toString s)
               | Posix.Process.W_SIGNALED s =>
                 Fail.fail ("pass", "run", "command signaled " ^ SysWord.toString (Posix.Signal.toWord s))
               | Posix.Process.W_STOPPED s =>
                 Fail.fail ("pass", "run", "command stop signaled " ^ SysWord.toString (Posix.Signal.toWord s)))
      end

  fun run (config, logger, cmd, args) = 
      let
        val cmd = Config.pathToHostString (config, cmd)
        val () = logger (config, String.concatWith (cmd::args, " "))
        val () = MLton.GC.collect ()
        val () = MLton.GC.pack ()
        val _  = runCmd (cmd, args, [], Config.silent config) handle any => 
                       Fail.fail ("Pass", "run", "Command could not be run: "^Exn.toString any)
        val () = MLton.GC.unpack ()
      in ()
      end

  fun runWithSh (config, logger, cmd, args) =
      run (config, logger, Path.fromString "bash", Config.pathToHostString (config, cmd) :: args)

end;
