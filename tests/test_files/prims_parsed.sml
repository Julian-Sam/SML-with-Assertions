functor MilPrimsF ( type fieldSize  ) = struct 

 type fieldSize = fieldSize 

 datatype vectorSize = Vs64 | Vs128 | Vs256 | Vs512 | Vs1024 

 datatype vectorDescriptor = Vd of { vectorSize : vectorSize , elementSize : fieldSize } 

 datatype floatPrecision = FpSingle | FpDouble 

 datatype intPrecision = IpArbitrary | IpFixed of IntArb.typ 

 datatype numericTyp = NtRat | NtInteger of intPrecision | NtFloat of floatPrecision 

 datatype divKind = DkT | DkF | DkE 

 datatype arithOp = AAbs | ANegate | ANegateSat | ADivide | ADiv of divKind | AMax | AMin | AMinus | AMinusSat | AMod of divKind | APlus | APlusSat | ATimes | ATimesSat | ADivMod of divKind 

 datatype floatOp = FaACos | FaASin | FaATan | FaCeil | FaCos | FaExp | FaFloor | FaLn | FaRcp | FaSin | FaSqrt | FaTan | FaTrunc | FaTanH | FaCosH | FaSinH | FaPow 

 datatype bitwiseOp = BNot | BAnd | BOr | BRotL | BRotR | BShiftL | BShiftR | BXor 

 datatype logicOp = LNot | LAnd | LOr | LXor | LEq 

 datatype compareOp = CEq | CNe | CLt | CLe 

 datatype nameOp = NGetString | NGetHash 

 datatype stringOp = SAllocate | SDeallocate | SGetLen | SGetChar | SSetChar | SEqual 

 datatype prim = PNumArith of { typ : numericTyp , operator : arithOp } | PFloatOp of { typ : floatPrecision , operator : floatOp } | PNumCompare of { typ : numericTyp , operator : compareOp } | PNumConvert of { to : numericTyp , from : numericTyp } | PNumCast of { to : numericTyp , from : numericTyp } | PBitwise of { typ : intPrecision , operator : bitwiseOp } | PBoolean of logicOp | PName of nameOp | PCString of stringOp | PPtrEq | PCondMov 

 datatype assoc = ALeft | ARight | AAny 

 datatype dataOp = DBroadcast | DVector | DSub of int | DPermute of int Vector.t | DBlend | DSplit | DConcat 

 datatype vector = ViPointwise of { descriptor : vectorDescriptor , masked : bool , operator : prim } | ViConvert of { to : { descriptor : vectorDescriptor , typ : numericTyp } , from : { descriptor : vectorDescriptor , typ : numericTyp } } | ViCast of { to : { descriptor : vectorDescriptor , typ : numericTyp } , from : { descriptor : vectorDescriptor , typ : numericTyp } } | ViCompare of { descriptor : vectorDescriptor , typ : numericTyp , operator : compareOp } | ViReduction of { descriptor : vectorDescriptor , associativity : assoc , operator : prim } | ViData of { descriptor : vectorDescriptor , operator : dataOp } | ViMaskData of { descriptor : vectorDescriptor , operator : dataOp } | ViMaskBoolean of { descriptor : vectorDescriptor , operator : logicOp } | ViMaskConvert of { to : vectorDescriptor , from : vectorDescriptor } 

 datatype runtime = RtFloatMk | RtWriteln | RtReadln | RtAssert | RtError | RtDebug | RtOpenOut | RtGetStdout | RtOutputByte | RtCloseOut | RtOpenIn | RtGetStdin | RtInputByte | RtInputString | RtInputAll | RtIsEOF | RtCloseIn | RtCommandLine | RtStringToNat | RtStringToFloat | RtFloatToString | RtFloatToStringI | RtRatNumerator | RtRatDenominator | RtEqual | RtDom | RtNub | RtRatToUIntpChecked | RtRatToString | RtStringToRat | RtResetTimer | RtGetTimer | RtVtuneAttach | RtVtuneDetach | RtArrayEval | RtIntegerHash 

 datatype t = Prim of prim | Runtime of runtime | Vector of vector  end 

