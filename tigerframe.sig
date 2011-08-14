signature tigerframe =
sig

type frame
type register

val rv : tigertemp.temp
val ov : tigertemp.temp
val fp : tigertemp.temp

datatype access = InFrame of int | InReg of tigertemp.temp

val fpPrev : int
val fpPrevLev : int
val newFrame : {name: tigertemp.label, formals: bool list} -> frame
val name : frame -> tigertemp.label
val actualLocalVar : frame -> int
val string : tigertemp.label * string -> string
val formals : frame -> access list
val allocArg : frame -> access
val allocLocal : frame -> bool -> access
val sp : tigertemp.temp
val wSz : int
val log2wSz : int
val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
val calleesaves : tigertemp.temp list

(*val exp : access -> tigertree.exp -> tigertree.exp*)

val registerList : tigertemp.temp list
val precoloredList : tigertemp.temp list

val argregs : tigertemp.temp list

val procEntryExit1 : frame * tigertree.stm -> tigertree.stm

(*
val externalCall : string * tigertree.exp list -> tigertree.exp
val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list 
*)

datatype frag = PROC of tigertree.stm * frame 
	| STRING of tigertemp.label * string

end
