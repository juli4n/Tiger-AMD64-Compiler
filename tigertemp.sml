structure tigertemp :> tigertemp =
struct 
	type label = string
	type temp = int 

local 

	val tarr = ref []
	val nt = ref 0
	val nl = ref 0
in

	fun newtemp () = 
		let
			val t = !nt
			val _ = tarr := !tarr @ ["T" ^ Int.toString(t)]
			val _ = nt := !nt + 1
		in t end
	
	fun namedtemp s =
		let
			val t = !nt
			val _ = tarr := !tarr @ [s]
			val _ = nt := !nt + 1
		in t end 
					
	fun tempastring n = List.nth (!tarr,n)
	
	fun tempaint n = n

	fun intatemp n = n	
(*
	fun newtemp () = 
					"T" ^ Int.toString (!nt)
					 before nt := !nt + 1
*)

	fun newlabel () =
					"L" ^ Int.toString (!nl)
					 before nl := !nl + 1


	fun namedlabel s = s
	fun labelastring s = s

	val compareTemp = Int.compare

	fun equalTemp (a,b) = (a = b)
	
(*	fun namedtemp s = s
	fun tempastring s = s
*)
end

end
