structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
	| TNil 
	| TInt
	| TString
	| TArray of Tipo * unique
	| TRecord of (string * Tipo * int) list * unique
	| TFunc of Tipo list * Tipo
	| TTipo of string * Tipo option ref


fun tipoPP t = case t of
				TUnit => "Unit"
				| TNil => "Nil"
				| TInt => "Int"
				| TString => "String"
				| TArray (t,u) => "Array of " ^ (tipoPP t)
				| TRecord (lfield,u) => 
					let val campos = List.map (fn (s,t,_) => s ^ ":" ^ tipoPP t) lfield
					in "Record of {" ^ (String.concat campos) ^ "}"
					end
				| TFunc (typesl,ty) => 
					let val tipos = List.map (fn t => (tipoPP t) ^ "->") typesl
					in "Function of: " ^ (String.concat tipos) ^ (tipoPP ty)
					end
				| TTipo (a,b) => "TTipo: " ^ a ^ " -> " ^ (case !b of
									   SOME s => "algo"
									   | NONE => "NONE")

end
