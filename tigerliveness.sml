structure tigerliveness :> tigerliveness =
struct

open tigerassem
open tigerutils
open tigertemp
open tigerintmap

open Intset

fun newset () = Intset.empty
fun unionset (a,b) = Intset.union (a,b)
fun diffset (a,b) = Intset.difference (a,b)
fun fromlistset l = Intset.addList (newset(),l)
fun eqset (a,b) = Intset.equal (a,b)

fun ppset a = 
	let val _ = print("{")
			val _ = Intset.app (fn x => print((tempastring (intatemp x))^",")) a
			val _ = print("}\n")
	in () end


fun livePP m = 
	let
		val lm = tigerintmap.listItems (m)
		fun f (i,{InS=in_i,OutS=out_i}) = 
			let val _ = print(Int.toString(i)^":\n")
					val _ = print("In:\t")
					val _ = ppset in_i
					val _ = print("Out:\t")
					val _ = ppset out_i
			in () end
	in List.app f lm end 


fun liveout (m:{InS: Intset.intset ,OutS: Intset.intset} tigerintmap.intMap) = 
	let
		val lm = tigerintmap.listItems m
		val out_i = case List.rev lm of
									(x::(i,{OutS=out_i,...})::ls) => out_i
								| _ => raise Fail "Error interno en liveout/tigerliveness"
	in out_i end
	

fun iguales (m1,m2) = 
	let 
		fun f ({InS=in1,OutS=out1},{InS=in2,OutS=out2}) = eqset (in1,in2) andalso eqset (out1,out2)
	in eqMaps f (m1,m2) end

fun uses i = 
	let 
		fun aux(OPER {src,...}) = src
			| aux (MOVEI {src,...}) = [src]
			| aux _ = []
	in fromlistset (List.map tempaint (aux i)) end

fun defs i = 
let
	fun aux (OPER {dst,...}) = dst
		| aux (MOVEI {dst,...}) = [dst]
		| aux _ = []
	in fromlistset (List.map tempaint (aux i)) end


fun liveness ls =
	let
		
		val linstr = (listTo (length ls))
		val indexi = ListPair.zip (linstr,ls) 	

		fun succs (pos,OPER {jump=SOME [l],...}) = 
				let
					fun busca (i,LABELI{lab,...}) = (labelastring lab) = (labelastring l)
						| busca (i,_) = false
				in case (List.find busca indexi) of 
						SOME (i,instr) => [i] @ (if pos < (length ls) then [pos+1] else [])
					| NONE => raise Fail "Error interno en liveness 2"
				end
			| succs (pos,_) = if pos < (length ls) then [pos+1] else []

		fun uniones (l,live) = 
				let
					fun aux (i,s) = 
						let 
							val {InS=in_i,OutS} = case findMap(live,i) of
										  SOME e => e
										| NONE => raise Fail "Error interno en uniones/tigerliveness"
						in unionset (s,in_i) end
				in List.foldl aux (newset()) l end
		 
		val (live,live') = (newMap(),newMap())
	
		val _ = List.app (fn i => insertMap(live,i,{InS=newset(),OutS=newset()})) linstr

		fun iter () = 
			let
				fun loop_n (i,instr) = 
					let
						val l as {InS=in_n,OutS=out_n} = case findMap (live,i) of
													SOME e => e
												| NONE => raise Fail "Error interno en loop_n/tigerliveness"
						val _ = insertMap (live',i,l)
			
						val in_temp = unionset (uses(instr),diffset (out_n,defs(instr)))
						val out_temp = uniones (succs(i,instr),live)
					in insertMap(live,i,{InS=in_temp,OutS=out_temp})  end
			in List.app loop_n indexi end

		fun fixed_point () = if iguales (live,live') then ()
												 else (iter ();fixed_point())

	in iter (); fixed_point(); live end

end
