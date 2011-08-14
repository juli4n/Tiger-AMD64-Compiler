signature tigertemp =
sig

	type label
	type temp

	val newtemp : unit -> temp
	val newlabel : unit -> label
	
	val namedlabel : string -> label
	val labelastring : label -> string
	val namedtemp : string	-> temp
	val tempastring : temp -> string
	val tempaint : temp -> int
	val intatemp : int -> temp

	val compareTemp : temp * temp -> order

	val equalTemp : temp * temp -> bool

end
