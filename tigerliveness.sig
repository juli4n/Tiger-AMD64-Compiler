signature tigerliveness =
sig

	val liveness : tigerassem.instr list -> 
				{InS: Intset.intset ,OutS: Intset.intset} tigerintmap.intMap

	val uses : tigerassem.instr -> Intset.intset

	val defs : tigerassem.instr -> Intset.intset

	val liveout : {InS: Intset.intset ,OutS: Intset.intset} tigerintmap.intMap -> Intset.intset

	val livePP : {InS: Intset.intset ,OutS: Intset.intset} tigerintmap.intMap -> unit

end
