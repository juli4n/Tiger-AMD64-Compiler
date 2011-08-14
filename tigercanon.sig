signature tigercanon = 
sig

datatype canonfrag = CPROC of tigertree.stm list * tigerframe.frame
										| CSTRING of tigertemp.label * string

val canonfrags : canonfrag list ref

val linearize : tigertree.stm -> tigertree.stm list

val basicBlocks :
	tigertree.stm list -> (tigertree.stm list list * tigertree.label)

val traceSchedule :
	tigertree.stm list list * tigertree.label -> tigertree.stm list

val canonize : tigertree.stm -> tigertree.stm list

val canoFrags : tigertranslate.frag list -> unit

end

