structure tigersres =
struct

open tigerabs
open tigertab
open tigertips

datatype EnvEntry =
	Var of {ty: Tipo, access: tigertranslate.access, level: int,ro: bool}
	| Func of {level: tigertranslate.level, label: string,
		formals: Tipo list, result: Tipo, extern: bool}


fun enventryPP (Var {ty,ro,...}) = "Var {ty =  " ^ (tigertips.tipoPP ty) ^ ", ro = " ^ (Bool.toString ro) ^ " }"
    | enventryPP (Func {formals,result,level,label,extern}) = 
	let val lista = List.map (fn t => tigertips.tipoPP t) formals
	in "Func { formals = " ^ (String.concat lista) ^ 
		      ", result = " ^ (tigertips.tipoPP result) ^ 
					", level = " ^ (Int.toString (#level(level))) ^
					" }"
	end

end
