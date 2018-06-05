structure AbSyn = 
struct
  datatype 
  	exp = 
				Int of string 
			  | Str of string
			  | Char of string
			  | Bool of string
			  | Real of string
			  | Id of string
			  | INFIXEXP of string * exp * exp 
			  | RAISEEXP of exp
			  | HANDLEEXP of exp * match list
			  | APPLY of exp * exp
			  | FUNC of int * match list
			  | WHILEDO of exp * exp
			  | CASEOF of exp * match list 
			  | ITE of exp * exp * exp
			  | TUPLES of exp list
			  | LISTS of exp list
			  | LETSTAT of dec * exp list
			  | FNOP of int * match list

  and 
  	match =     MATCH of pat * exp

  and 
  	pat = 		INTPAT of string
  			  | CHARPAT of string
  			  | STRPAT of string
  			  | REALPAT of string
  			  | BOOLPAT of string
  			  | IDPAT of string
  			  | WILD 
  			  | INFIXPAT of string * pat * pat
  			  | ASPAT of string * pat
  and 
  	dec = 		VALB of pat * exp
  			  | FUNCTB of string * match list
  			  | FUNCTBs of (string * match list) list
	
end