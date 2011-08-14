signature tigerassem =
sig

	type reg

	datatype instr = OPER of { assem: string,
														 dst: tigertemp.temp list,
														 src: tigertemp.temp list,	
														 jump: tigertemp.label list option }
									| LABELI of { assem: string,
															 lab: tigertemp.label } 
									| MOVEI of { assem: string,
														 	dst: tigertemp.temp,
														 	src: tigertemp.temp }

	val codegen : (tigertree.stm * tigerframe.frame) -> instr list

	val operPP : instr -> unit

	val genera : tigercanon.canonfrag list -> {frag:tigercanon.canonfrag,code:instr list option} list

	val format : (tigertemp.temp -> string) -> instr -> string

	val isMoveI : instr -> bool
	val isJump : instr -> bool

	val procEntryExit3 : tigerframe.frame * instr list -> instr list
	
	val printFun : tigerframe.frame * instr list -> unit

	val printString : tigertemp.label * string -> unit

	val instrPP  : instr list -> unit
end	
