structure tigerliv =
struct

open tigertemp
open tigerset
open tigermap
open tigerassem
open tigerutils

fun liveness ls = 
let

(*	val mark = newMap Int.compare
	val sorted = newMap Int.compare
	val predMap = newMap Int.compare 
*)
	val succMap = newMap Int.compare

	val cantInstr = List.length ls
	val n = ref (cantInstr-1)
	val li = 0::(listTo(cantInstr-1))
	val indexli = ListPair.zip (li,ls)

	val (outMap,outMap') = (newMap Int.compare,newMap Int.compare)
	val (inMap,inMap') = (newMap Int.compare, newMap Int.compare)

	val _ = List.app (fn i => insertMap(outMap,i,newSet compareTemp)) li
	val _ = List.app (fn i => insertMap(outMap',i,newSet compareTemp)) li
	val _ = List.app (fn i => insertMap(inMap,i,newSet compareTemp)) li
	val _ = List.app (fn i => insertMap(inMap',i,newSet compareTemp)) li
	
	fun uses i =
  	let
			val r = newSet compareTemp
    	fun aux(OPER {src,...}) = src
      	| aux (MOVEI {src,...}) = [src]
      	| aux _ = []
  	in addListSet (r,aux i) ; r end

	fun defs i =
		let
			val r = newSet compareTemp
 			fun aux (OPER {dst,...}) = dst
    		| aux (MOVEI {dst,...}) = [dst]
    		| aux _ = []
  	in addListSet (r, aux i) ; r end

	fun pred pos = 
		let
			val l = newSet Int.compare
			val instr = List.nth(ls,pos)
		in case instr of
				LABELI{lab,...} => 
					let
						fun busca [] = []
							|	busca ((i,OPER{jump=SOME[l],...})::le) = if ((labelastring l) = (labelastring lab)) then 
																												 i::(busca le) else (busca le)
							| busca (_::le) = busca le
						val p = if pos<>0 then (pos-1)::busca indexli else busca indexli
						val _ = addListSet (l,p)
					in l end
			| _ => ((if pos=0 then () else addElemSet(l,pos-1)) ; l)
		end

fun succs pos =
	let
		val ts = newSet Int.compare
		val instr = List.nth(ls,pos)
	in case instr of
		OPER {jump=SOME [l],...} =>
        let
          fun busca (i,LABELI{lab,...}) = (labelastring lab) = (labelastring l)
            | busca (i,_) = false
        in (case (List.find busca indexli) of
            SOME (i,instr) => (addListSet(ts,([i] @ (if pos < (length ls -1) then [pos+1] else []))) ; ts)
          | NONE => raise Fail "Error interno en liveness 2")
        end
    | _ => (addListSet (ts,if pos < (length ls -1) then [pos+1] else []) ; ts)
	end


(* fun calc_pred () = List.map (fn x => insertMap(predMap,x,pred x)) li  *)
fun calc_succ () = List.map (fn x => insertMap(succMap,x,succs x)) li

(*
fun dfs i = if (findMap(mark,i)=false) then 
						(insertMap(mark,i,true);
						print("entra "^(Int.toString i)); 
						forallSet dfs (findMap (predMap,i));
						print("sale "^(Int.toString i)); 
						 insertMap(sorted,!n,i);
						 n := !n - 1)
					  else ()

fun topSort () =
let
	val _ = List.app (fn (i,instr) => insertMap (mark,i,false)) indexli
	val exit_node = cantInstr -1
in dfs(exit_node) end
*)
fun no_change (a,b) = ListPair.all (fn ((c1,v1),(c2,v2)) => equalSet (v1,v2)) (listItemMap a, listItemMap b)

fun loop_n n =
	let
		val _ = insertMap(outMap',n,findMap (outMap,n))
		val _ = insertMap(inMap',n,findMap (inMap,n))
		val instr = List.nth (ls,n)
		val _ = insertMap(inMap,n,unionSet(uses(instr),differenceSet(findMap(outMap,n),defs(instr))))
		val _ = let val t = newSet compareTemp in
							forallSet (fn s => unionInSet (t,findMap(inMap,s))) (findMap (succMap,n));
							insertMap(outMap,n,t)
						end
	in () end

(*
fun loop_n n =
	let
		val _ = insertMap(outMap',n,findMap (outMap,n))
		val _ = insertMap(inMap',n,findMap (inMap,n))
		val instr = List.nth (ls,n)
		val _ = insertMap(outMap,n,unionSet(uses(instr),differenceSet(findMap(inMap,n),defs(instr))))
		val _ = let val t = newSet compareTemp in
							forallSet (fn s => unionInSet (t,findMap(outMap,s))) (findMap (predMap,n));
							insertMap(inMap,n,t)
						end
	in () end
*)
fun fixedPoint () =
	(List.app loop_n li;
	if (no_change(outMap,outMap')  andalso no_change(inMap,inMap')) then ()
	else fixedPoint ())

in
	calc_succ(); 
(*	calc_pred(); *)
	fixedPoint();
(*	mapPP Int.toString (setPP tempastring) outMap;
	mapPP Int.toString (setPP tempastring) inMap; *)
	outMap
end
	
(*
fun loop_i i = 
	let
		val n = findMap (sorted,i)
		val _ = insertMap(out',n,findMap (out,n))
		val instr = List.nth(ls,n)
		val in_n' = newSet compareTemp
		val _ = forallSet (fn p => unionInSet(in_n',findMap(out,p))) (findMap(predMap,n))(*(pred n)*)
		val _ = insertMap (out,n,unionSet(uses(instr),differenceSet(in_n',defs(instr))))
	in () end




fun fixedPoint () =
		(List.app loop_i li;
		if no_change (out,out') then () else fixedPoint ())
		
in 	
		calc_pred();
		calc_succ();
		topSort (); 
		fixedPoint ();

		mapPP Int.toString (setPP Int.toString) predMap;
		mapPP Int.toString (setPP Int.toString) succMap;
		mapPP Int.toString Int.toString sorted;
		mapPP Int.toString (setPP tempastring) out;
		out 
end
*)
end
