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


signature MIL_SHAPE_ANALYSIS =
sig
  type t
  type env

  datatype value = VSymb of Identifier.variable
                 | VInt of Int32.t

  val debugs : Config.Debug.debug list

  val length : t * Identifier.variable -> value option
  val isScalar : t * Identifier.variable -> bool
  val isArray : t * Identifier.variable -> bool

  val equalLength : t * Identifier.variable * Identifier.variable -> bool

  val layout : t -> Layout.t

  val program : env * Mil.t -> t
end
                           
functor MilShapeAnalysisF (
  type env

  val getConfig : env -> Config.t
  val passname : string
  val indent : int
) :> MIL_SHAPE_ANALYSIS where type env = env = 

struct
  val myPassname = passname ^ ":SAnlz"

  val (debugPassD, debugPass) = 
    Config.Debug.mk (myPassname, "debug the shape analysis module")

  type env = env

  structure L = Layout
  structure LU = LayoutUtils
  structure M = Mil

  structure I = Identifier
  structure LS = Identifier.LabelSet
  structure LD = Identifier.LabelDict
  structure VD = Identifier.VariableDict
  structure P = Prims
 
  (*
   * datastructure encoding shape information
   *)
  datatype value = VSymb of Identifier.variable
                 | VInt of Int32.t

  datatype shape = SAny (* top *)
                 | SDynamic of shape (* element shape *)
                 | SKnown of value * shape (* length, el. shape *)
                 | SScalar (* not a tuple *)
                 | SUnknown (* bottom *)

  type t = shape VD.t 

  fun sMin (_, m) = m
  fun sMax (m, _) = m

  (*
   * debugging functions
   *)
  fun dbgPrint (env, msg) = 
      if Config.debug andalso (debugPass (getConfig env)) then print msg else ()

  (*
   * functions on shape info
   *)
  fun layoutShapeInfo (s) =
      let
        fun value (v) =
            case v
             of VSymb v => Identifier.layoutVariable' (v)
              | VInt i => Int32.layout (i)
        fun inner (s) =
            case s
             of SAny => [L.str "..."]
              | SDynamic n => (L.str ".")::(inner n)
              | SKnown (v, n) => (value v)::(inner n)
              | SScalar => []
              | SUnknown => [L.str "?"]
      in
        LU.bracketSeq (inner s)
      end

  fun layoutShapeInfoP (s) =
      L.seq [layoutShapeInfo (sMax (s)), 
             L.str "<", 
             layoutShapeInfo (sMin (s))]

  fun shapeToString (s) = LU.toString (layoutShapeInfoP (s))

  fun valueEq (v1, v2) =
      case (v1, v2) 
       of (VSymb var1, VSymb var2) => var1 = var2
        | (VInt i1, VInt i2) => i1 = i2
        | _ => false

  fun shpEq (s1, s2) =
      case (s1, s2)
       of (SAny, SAny) => true
        | (SScalar, SScalar) => true
        | (SUnknown, SUnknown) => true
        | (SDynamic (a), SDynamic (b)) => shpEq (a, b)
        | (SKnown (l1, a), SKnown (l2, b)) => 
          (valueEq (l1, l2)) andalso (shpEq (a, b))
        | _ => false

  fun lub (s1, s2) =
      case (s1, s2) 
       of (SScalar, SScalar) => SScalar
        | (SDynamic (a), SDynamic (b)) => SDynamic (lub (a, b))
        | (SDynamic (a), SKnown (_, b)) => SDynamic (lub (a, b))
        | (SKnown (_, a), SDynamic (b)) => SDynamic (lub (a, b))
        | (SKnown (l1, a), SKnown (l2, b)) => 
          if (valueEq (l1, l2)) then SKnown (l1, lub (a, b))
          else SDynamic (lub (a, b))
        | (other, SUnknown) => other
        | (SUnknown, other) => other
        | _ => SAny

  fun lubP ((a1, a2), (b1, b2)) = (lub (a1, b1), lub (a2, b2))

  fun glb (s1, s2) =
      case (s1, s2) 
       of (SScalar, SScalar) => SScalar
        | (SDynamic a, SDynamic b) => SDynamic (glb (a, b))
        | (SDynamic a, SKnown (i, b)) => SKnown (i, glb (a, b))
        | (SKnown (i, a), SDynamic b) => SKnown (i, glb (a, b))
        | (SKnown (i1, a), SKnown (i2, b)) => 
          if (valueEq (i1, i2)) then SKnown (i1, glb (a, b))
          else SUnknown
        | (other, SAny) => other
        | (SAny, other) => other
        | _ => SUnknown

  fun glbP ((a1, a2), (b1, b2)) = (glb (a1, b1), glb (a2, b2))

  fun merge ((max1, min1), (max2, min2)) = 
      let 
        val max = glb (max1, max2)
        val min = lub (min1, min2)
      in
        (max, glb (max, min))
      end

  fun unknownShape (v) = (SAny, SUnknown)

  (*
   * shape inference from different sources
   *)

  (*
   * most constants are scalars. For all others I think the
   * concept shape does not apply.
   *)
  fun constantToShape (c) =
      case c
       of M.CIntegral _ => (SScalar, SScalar)
        | M.CFloat _ => (SScalar, SScalar)
        | M.CDouble _ => (SScalar, SScalar)
        | _ => (SAny, SUnknown)

  (*
   * simply lookup the shape information for either the constant or
   * the variable. I assume that all type coercions are shape
   * preserving.
   *)
  and simpleToShape (st, s) =
      case s
       of M.SConstant c => constantToShape (c)
        | M.SVariable vv => st (vv)

  (*
   * typ information provides upper bounds for arrays and
   * for some scalars. 
   *
   * The pObj and typ option seem somewhat redundant. I
   * guess it's safe to assume the glb of all of them.
   *)
  and typToShape (t : M.typ) =
      case t
       of M.TIntegral _ => (SScalar, SUnknown)
        | M.TFloat => (SScalar, SUnknown)
        | M.TDouble => (SScalar, SUnknown)
        | M.TTuple {pok, fixed, array} => 
          let
            val b = pObjToShape pok
            val arraytyp = SOME (#1 array)
            val tshp = Utils.Option.get (Option.map (arraytyp, typToShape), (SAny, SUnknown))
            val a = (SDynamic (sMax (tshp)), SUnknown)
          in 
            merge (a, b)
          end
        | M.TPRef typ => typToShape typ
(*      | M.TPObj pobj => pObjToShape (pobj) *)
        | _ => (SAny, SUnknown)

  (*
   * the type information in the p object descriptor gives 
   * an upper approximation of the nested array shape, i.e., we
   * might be able to compute the nesting depth of an 
   * array up to a certain level. For scalars, the pObj information
   * is definite.
   *)
  and pObjToShape (pObj) = 
      case pObj 
       of M.PokRat => (SScalar, SUnknown)
        | M.PokFloat => (SScalar, SUnknown)
        | M.PokDouble => (SScalar, SUnknown)
(*        | M.PokOArray => (SDynamic (sMax (typToShape t)), SUnknown)*)
(*        | M.PokIArray => (SDynamic (sMax (typToShape t)), SUnknown)*)
(*        | M.PArrayIdx t => (SDynamic (sMax (typToShape t)), SUnknown)*)
(*        | M.PtArrayFixed ts => 
          (SKnown (VInt (Vector.length ts),
                   (Vector.fold (ts, 
                                 SUnknown, 
                                 fn (a, b) => lub (sMax (typToShape a), b)))), 
           SUnknown)
        | M.PtArrayIdxFixed (_, ts) => 
          (SKnown (VInt (Vector.length ts),
                   (Vector.fold (ts, 
                                 SUnknown,
                                 fn (a, b) => lub (sMax (typToShape a), b)))),
           SUnknown)
*)
        | _ => (SAny, SUnknown)
 
  (*
   * given the length as an operand and the element shape, returns 
   * the shape of the resulting array
   *)
  fun nestShape (len : M.simple, nest : shape) =
      case len
       of M.SConstant (M.CIntegral i) => 
          SKnown (VInt (Int32.fromIntInf (IntArb.toIntInf i)), nest)
        | M.SVariable v => SKnown (VSymb v, nest)
        | _ => SDynamic nest

  fun nestShapeP (len : M.simple, (s1 : shape, s2 : shape)) = 
      (nestShape (len, s1), nestShape (len, s2))

  (*
   * returns the nested shape info or def if none is known
   *)
  fun denestShape (shp, def) =
      case shp
       of SDynamic n => n
        | SKnown (_, n) => n
        | _ => def
       
  fun denestShapeP ((s1, s2), (d1, d2)) = (denestShape (s1, d1), 
                                           denestShape (s2, d2))
  (*
   * collect information for different MIL components
   *)
  fun deriveGlobal (env, st, g) =
      case g 
       of M.GSimple s => simpleToShape (st, s)
(*        | M.GTuple (pobj, ops) => 
          Utils.Option.get (Option.map (pobj, pObjToShape), (SAny, SUnknown))*)
        | M.GRat _ => (SScalar, SScalar)
        (* for all others there is no possible shape value *)
        | _ => (SAny, SAny)

  fun derivePrim (env, st, v, p, ops) = 
      case p
       of P.Prim _ => (SScalar, SScalar)
        (* all primitives operate on scalars *)
        | _ => (SAny, SAny)
        (* for the rest I don't know *)
      

  fun deriveInstr (env, st, dests, rhs) = 
      let
        val some = Vector.new1
        val none = Vector.new0 ()
        val dest = fn () => Vector.sub (dests, 0)
        val res = 
            case rhs 
             of M.RhsSimple s => some (dest (), (simpleToShape (st, s)))
              | M.RhsPrim {prim, createThunks, args} => 
                (case Utils.Vector.lookup (dests, 0)
                  of SOME vv => some (vv, derivePrim (env, st, vv, prim, args))
                   | NONE => none)
                  (*        | M.RhsPRat _ => SOME (Option.valOf v, (SScalar, SScalar))*)
              | M.RhsTuple {vtDesc, inits} =>
                let 
                  val vtd as M.VTD {pok, fixed, array} = vtDesc
                                                         
                  val () = dbgPrint (env, "TUPLE ->")
                  val pshp = pObjToShape (pok)
                  val () = dbgPrint (env, "PSHP: " ^ shapeToString (pshp))
                  fun simpleToShape' s = simpleToShape (st, s)
                  val tshp = (Vector.fold (inits, 
                                           SUnknown,
                                        fn (a, b) => lub (sMax (simpleToShape' a), b)),
                              SUnknown)
                  val () = dbgPrint (env, "TSHP: " ^ shapeToString (tshp))
                in 
                  some (dest (), (merge (pshp, tshp)))
                end
                  (*        | M.RhsTupleSub tf =>
                              let
                                val tfs as M.TF {tupDesc, tup, field} = tf
                                                                        
                                val () = dbgPrint (env, "TUPLESUB ->")
                                val shp = case dyn
                                           of SOME _ => denestShapeP (st (s), (SAny, SUnknown))
                                            | NONE => (SAny, SUnknown)
                              in SOME (Option.valOf v, shp)
                                      end*)
                  (*        | M.RhsTupleSet (v, f, dyn, s) =>
                              let
                                val () = dbgPrint (env, "TUPLESET ->")
                                val min = SDynamic (sMin (simpleToShape (st, s)))
                              in
                                (*
                                 * we only get a lower approximation here as this updates
                                 * a single element of the array only. The maximum of the
                                 * overall array might be weaker.
                                 *)
                                  SOME (v, (SAny, min))
                              end
                   *)
              | _ => Vector.map (dests, fn (vv) => (vv, (SAny, SAny)))
                                (* for the rest I don't know *)
      in res
      end

  (* for functions in general I don't know *)
  fun deriveFunction (env, st, _, p, _, _) = 
      Vector.fromList (List.duplicate (p, fn () => (SAny, SUnknown)))

  fun deriveBlock (_, dict, _, args) =
      Vector.map (args, fn (arg) => simpleToShape (dict, arg))

  fun length (si, v) =
      case VD.lookup (si, v) 
       of SOME (SKnown (l, _)) => SOME l
        | _ => NONE

  fun isScalar (si, v) =
      case VD.lookup (si, v)
       of SOME (SScalar) => true
        | _ => false

  fun isArray (si, v) =
      case VD.lookup (si, v)
       of SOME (SKnown _) => true
        | SOME (SDynamic _) => true
        | _ => false

  fun equalLength (si, v1, v2) =
      case (length (si, v1), length (si, v2)) 
       of (SOME (VInt i1), SOME (VInt i2)) => i1 = i2
        | (SOME (VSymb s1), SOME (VSymb s2)) => s1 = s2
        | _ => false

  fun layout (shp) = VD.layout (shp, fn (k, s) => L.seq [I.layoutVariable' k,
                                                         L.str " :: ",
                                                         layoutShapeInfo s])

  fun layout' (shp) = VD.layout (shp, fn (k, s) => L.seq [I.layoutVariable' k,
                                                          L.str " :: ",
                                                          layoutShapeInfoP s])

  structure ShapeAnalysis = MilDataFlowAnalysisF (
                              type env = env
                              type info = shape * shape
                              structure MilCfg = MilCfg
                              val getConfig = getConfig
                              val passname = myPassname
                              val indent = indent + 2
                              val deriveConstant = 
                                  fn (_, c) => constantToShape (c)
                              val deriveInstr = deriveInstr
                              val deriveGlobal = deriveGlobal
                              val deriveFunction = deriveFunction
                              val deriveBlock = deriveBlock
                              val emptyInfo = unknownShape
                              val mergeInfo = fn (_, a, b) => merge (a, b)
                              val coerceInfo = fn (_, _, x) => x
                              val equalInfo = 
                                  fn (_, (_, a), (_, b)) => shpEq (a, b)
                              val layoutInfo = 
                                  fn (_, a) => layoutShapeInfoP (a)
                            )

  fun program (env, m) = 
      let
        val sinfo = ShapeAnalysis.program (env, m)
        val () = dbgPrint (env, L.toString (layout' sinfo))
      in  
        VD.map (sinfo, fn (_, (_, min)) => min)
      end

  val debugs = [debugPassD] @ ShapeAnalysis.debugs
end 
