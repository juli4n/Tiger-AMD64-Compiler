structure tigercoloring =
struct

open tigerset
open tigermap
open tigerframe
open tigerassem
open tigerutils
open tigertemp
open tigerstack
open tigerliv


fun coloreo (l,f) = 
let

val linstr = ref l
val frame = ref f

val workListMoves = newSet Int.compare  (* Conjunto de intrucciones (MOVEI) para posible coaleascing *)
val coalescedMoves = newSet Int.compare 
val constrainedMoves = newSet Int.compare
val frozenMoves = newSet Int.compare
val activeMoves = newSet Int.compare

val moveList = newMap compareTemp (* Mapeo de nodos a instrucciones moves (int) asociadas al nodo *)

fun printMovesStructs () =
	( print("\nworklistMoves:\n" ^ (setPP Int.toString workListMoves));
	 print("\ncoalescedMoves:\n" ^ (setPP Int.toString coalescedMoves));
	 print("\nconstrainedMoves:\n" ^ (setPP Int.toString constrainedMoves));
	 print("\nfrozenMoves:\n" ^ (setPP Int.toString frozenMoves ));
	 print("\nactiveMoves:\n" ^ (setPP Int.toString activeMoves));
	 print("\nMoveList:\n") ; (mapPP tempastring (setPP Int.toString) moveList)
	)

val spillWorkList = newSet compareTemp
val freezeWorkList = newSet compareTemp
val simplifyWorkList = newSet compareTemp

fun printWorkList () =
	( (print ("\nsimplifyWorkList:\n"); print (setPP tempastring simplifyWorkList));
		(print ("\nWorkListMoves:\n"); print (setPP Int.toString workListMoves));
		(print ("\nfreezeWorkList:\n"); print (setPP tempastring freezeWorkList));
		(print ("\nspillWorkList:\n"); print (setPP tempastring spillWorkList)))



val coalescedNodes = newSet compareTemp
val coloredNodes = newSet compareTemp
val spilledNodes = newSet compareTemp

val selectStack = newstack ()

val alias = newMap compareTemp

val color = newMap compareTemp

val adjList = newMap compareTemp (* Mapeo de temp a temp que interfieren *)

fun updateMovesSets n =
		let 
			fun updatePos l = List.map (fn x => if x >= n then x+1 else x) l
			fun updateSet l = let val l' = listItemSet l
												in (setEmptySet l;
												 		addListSet (l,updatePos l');
														l)
												end
			val worklm = listItemSet workListMoves
			val _ = (setEmptySet (workListMoves);addListSet(workListMoves,updatePos (worklm)))
			val coalm = listItemSet coalescedMoves
			val _ = (setEmptySet (coalescedMoves);addListSet(coalescedMoves,updatePos coalm))
			val consm = listItemSet constrainedMoves
			val _ = (setEmptySet (constrainedMoves);addListSet(constrainedMoves,updatePos consm))
			val frozm = listItemSet frozenMoves
			val _ = (setEmptySet (frozenMoves);addListSet(frozenMoves,updatePos frozm))
			val actm = listItemSet activeMoves
			val _ = (setEmptySet (activeMoves);addListSet(activeMoves,updatePos actm))

			val lm = listItemMap moveList 
			val _ = List.app (fn (k,v) => insertMap(moveList,k,updateSet v)) lm 

		in () end


val _ = let
			val l = ListPair.zip (0::(listTo (length(tigerframe.registerList)-1)),tigerframe.registerList)
			in List.app (fn (v,k) => insertMap(color,k,v)) l end

val adjSet = let												(* Conjunto de nodos adjacentes en el grafo de interferencia *)
			fun compareNodes ((a,b),(c,d)) = case (compareTemp(a,c),compareTemp(b,d)) of
													(EQUAL,EQUAL) => EQUAL
												| (EQUAL,a) => a
												| (a,_) => a
			in newSet compareNodes end

fun graphPP (u,v) = "(" ^ (tempastring u) ^ "," ^ (tempastring v) ^ ")"

val precolored = 
	let val t = newSet compareTemp
			val _ = addListSet (t,tigerframe.registerList)
	in t end

val degree = newMap compareTemp (* Mapeo de nodos al grado (int) del nodo *)

val _ = List.app (fn t => insertMap (degree,t,9999)) tigerframe.registerList 

val initial = newSet compareTemp

val k = List.length (tigerframe.registerList)

val single = singletonSet compareTemp 

fun findSetOfMap (m,k,neutro) = case peekMap (m,k) of
									 SOME e => e
								 | NONE => neutro


fun addEdge (u,v) = 
		if (not (memberSet(adjSet,(u,v))) andalso not (equalTemp (u,v))) then
				(addListSet (adjSet,[(u,v),(v,u)]);
				(if (not (memberSet(precolored,u))) then
						((insertMap (adjList,u, unionSet (findSetOfMap (adjList,u,newSet compareTemp),single v)));
						(insertMap (degree,u, (findSetOfMap (degree,u,0)) + 1)))
				else ());
				(if (not (memberSet(precolored,v))) then
						((insertMap (adjList,v, unionSet (findSetOfMap (adjList,v,newSet compareTemp),single u)));
						(insertMap (degree,v, (findSetOfMap (degree,v,0)) + 1)))
				else ()))
		else ()

fun uses i =
  let
		val t = newSet compareTemp
    fun aux(OPER {src,...}) = src
      | aux (MOVEI {src,...}) = [src]
      | aux _ = []
  in addListSet (t, aux i) ; t end

fun defs i =
let
	val t = newSet compareTemp
  fun aux (OPER {dst,...}) = dst
    | aux (MOVEI {dst,...}) = [dst]
    | aux _ = []
  in addListSet (t, aux i) ; t end

val live = newSet compareTemp

(* Calculo los temps que van a initial antes de arrancar a iterar *)
val _ = List.app (fn i=> unionInSet(initial,unionSet(uses i,defs i))) (!linstr)

val registers = 
	let val t = newSet compareTemp
	in addListSet(t,registerList) ; t end

val _ = differenceInSet (initial,registers)

fun degreeInvariant () = 
	let
		val t = unionSet(simplifyWorkList,unionSet(freezeWorkList,spillWorkList))
	in	
		forallSet (fn u => 
			if findMap(degree,u) <> (cardinalSet(intersectionSet(findMap(adjList,u),unionSet(precolored,t)))) then
				(printWorkList();
				mapPP tempastring (setPP tempastring) adjList;
				mapPP tempastring Int.toString degree; 
				print ("---------------\n");
				print ("EL temporal "^(tempastring u)^" no cumple la invariante!\n");
				print ("degree["^(tempastring u)^"] = "^(Int.toString (findMap(degree,u)))^"\n");
				print ("|adjList["^(tempastring u)^"... = " ^ 
					Int.toString(cardinalSet(intersectionSet(findMap(adjList,u),unionSet(precolored,t)))));
				raise Fail " no cumple degree Invariant!")
			else ()) t
	end


fun liveOutInit () = (setEmptySet(live); addListSet (live,[fp,sp,rv]@calleesaves))

fun build () = 
let
		val livenessResult = liveness (!linstr)
		val _ = liveOutInit ()			(* Conjunto de Temporales *)
		fun loop (i,instr) = 
			let 
	
				val use_i = uses (instr)
				val def_i = defs (instr)
			
				val _ = if isJump(instr) then 
									(setEmptySet live;
									unionInSet (live,findMap (livenessResult,i)))
								else ()
															(* Agrego todos los nodos definidos y usados a initial, al final
																 le resto los precoloreados *)
 
				val _ = if isMoveI (instr) then (
									differenceInSet (live,use_i);
									forallSet (fn n => 
											insertMap(moveList,n,	unionSet (findSetOfMap (moveList,n,newSet Int.compare), 
																											singletonSet Int.compare i)))
											(unionSet (use_i,def_i));
									unionInSet (workListMoves, singletonSet Int.compare i))
								else ()
				val _ = unionInSet (live,def_i)	
				val _ = forallSet (fn d => forallSet (fn l => addEdge (l,d)) live) def_i
				val _ = (differenceInSet (live,def_i) ; unionInSet (live,use_i))
		in () end							

						 								
		val l' = List.rev (ListPair.zip (0::(listTo((length (!linstr))-1)),(!linstr)))
		val _ = List.app loop l'
			
(*
		val _ = print(" ----------- Iteracion ------------\n" )
		val _ = instrPP (!linstr)
		val _ = print("Liveness:\n")
		val _ = mapPP Int.toString (setPP tempastring) livenessResult
		val _ = (print ("\nInitial:\n"); print (setPP tempastring initial))
		val _ = (print ("\nPrecolored:\n"); print (setPP tempastring precolored))
		val _ = (print ("\ndegree:\n"); mapPP tempastring Int.toString degree)
		val _ = (print ("AdjList:\n"); mapPP tempastring (setPP tempastring) adjList) 
		val _ = (print ("MoveList:\n"); mapPP tempastring (setPP Int.toString) moveList) 
		val _ = print("\nGrafo de adjacencia:\n")
		val _ =  print (setPP graphPP adjSet ) 
		val _ = printMovesStructs() 
*)
in () end 

fun nodeMoves n = case peekMap (moveList,n) of
							  		SOME a => intersectionSet (a, unionSet (activeMoves,workListMoves))
									| NONE => newSet Int.compare 
	
fun moveRelated n = not (isEmptySet(nodeMoves n))

fun makeWorkList () =
	let
		fun f n = 
			let
				val _ = differenceInSet (initial,single n)
				val degree_n = case peekMap(degree,n) of
									SOME g => g
								| NONE => raise Fail "Error interno en tigercoloring, inconsistencia degree con"
			in if degree_n >= k then 	
					unionInSet(spillWorkList,single n)
				 else if moveRelated(n) then 
					unionInSet(freezeWorkList,single n)
				 else 
					unionInSet(simplifyWorkList,single n)
			end
	in (forallSet f initial) end

fun adjacent n = 
	let 
		val t = newSet compareTemp
		val _ = addListSet (t, stackToList selectStack)
	in
		case peekMap (adjList,n) of
									  SOME a => differenceSet (a,unionSet(t,coalescedNodes))
									| NONE => newSet compareTemp 
	end

fun enableMoves nodes = 
	forallSet (fn n => 
					forallSet (fn m => 
							if memberSet(activeMoves,m) then 
								(differenceInSet(activeMoves,singletonSet Int.compare m);
								 unionInSet(workListMoves,singletonSet Int.compare m))
							else ()) (nodeMoves n)) nodes


fun decrementDegree m =
	let
		val d = case peekMap(degree,m) of
						SOME a => a
						| NONE => raise Fail "Error interno en degree, inconsistencia"
		val _ = insertMap (degree,m,d-1)
	in
		if d = k then 
				(enableMoves(unionSet(single m,adjacent m));
				differenceInSet(spillWorkList,single m);
				if moveRelated(m) then 
					unionInSet(freezeWorkList,single m)
				else
					unionInSet(simplifyWorkList,single m))
		else ()
	end


fun simplify () = 
	let
		val n = elemFromSet simplifyWorkList
		val _ = differenceInSet(simplifyWorkList, single n)
		val _ = push selectStack n
(*		val _ = (print("Simplify:\n") ; print("Temp:" ^ (tempastring n) ^ "\n")) *)
	in forallSet decrementDegree (adjacent n) end


fun addWorkList u = 
	let
		val degree_u = case peekMap(degree,u) of	
								SOME e => e
							| NONE => raise Fail "error en addWorkList, inconcistencia en degree"
	in 
		if (not (memberSet (precolored,u)) andalso not(moveRelated u) andalso (degree_u < k)) then
		(differenceInSet(freezeWorkList,single u);
		 unionInSet(simplifyWorkList,single u))
		else ()
	end

fun ok (t,r) =
	let
		val degree_t = case peekMap(degree,t) of	
								SOME e => e
							| NONE => raise Fail ("error en ok, inconcistencia en degree" ^ (tempastring t))
	in 
		((degree_t < k) orelse (memberSet (precolored,t)) orelse (memberSet (adjSet,(t,r))))
	end

fun conservative nodes = 
	let
		val t = ref 0
		fun degree_n n = case peekMap(degree,n) of	
								SOME e => e
							| NONE => raise Fail "error en conservative, inconcistencia en degree"
		val _ = forallSet (fn n => if (degree_n n) >= k then t := !t + 1 else ()) nodes
	in ((!t) < k) end

fun getAlias n = if memberSet(coalescedNodes,n) then 
										let
											val alias_n = case peekMap(alias,n) of
																		SOME e => e
																	| NONE => raise Fail "error en getalias , inconsistencia en alias"
										in getAlias alias_n end
									else n


fun combine (u,v) = ((if memberSet(freezeWorkList,v) then differenceSet(freezeWorkList,single v)
										else differenceSet(spillWorkList,single v));
										unionInSet(coalescedNodes,single v); 
										insertMap(alias,v,u);
										insertMap(moveList,u,unionSet (findSetOfMap(moveList,u,newSet Int.compare),
																									findMap (moveList,v)));

										enableMoves(single v); 
										forallSet (fn t => (addEdge(t,u);decrementDegree t)) (adjacent v);
										if ((findMap(degree,u) >= k) andalso (memberSet(freezeWorkList,u))) then 
											(differenceInSet(freezeWorkList,single u);
											 unionInSet(spillWorkList,single u))
										else () )
 								


fun coalesce () = 
	let
		val m = elemFromSet workListMoves
		val instr = List.nth (!linstr,m)
		val (x,y) = case instr of
						MOVEI {src,dst,...} => (getAlias src, getAlias dst)
					| _ => ((operPP instr) ; raise Fail ("1error interno en coalesce: No es un MOVE"))
		val (u,v) = (ref x, ref y)	

	in 
		(if memberSet(precolored,y) then (u:=y;v:=x) else (u:=x;v:=y);
		 differenceInSet(workListMoves,singletonSet Int.compare m);
		 if equalTemp(!u,!v) then
				(unionInSet(coalescedMoves,singletonSet Int.compare m);
				 addWorkList (!u))
		 else if (memberSet(precolored,!v) orelse memberSet(adjSet,(!u,!v))) then
					(unionInSet(constrainedMoves,singletonSet Int.compare m);
					 addWorkList(!u);
					 addWorkList(!v))
	   else if ((memberSet (precolored,!u) andalso (List.all (fn t => ok(t,!u)) (listItemSet (adjacent (!v)))))
							orelse ((not (memberSet(precolored,!u))) 
							andalso (conservative (unionSet(adjacent (!u),adjacent (!v)))))) then
					(unionInSet(coalescedMoves,singletonSet Int.compare m);
		(*		 print ("haciendo coalescing de nodos:"^(tempastring(!u))^" y "^(tempastring(!v))); *)
					 combine(!u,!v);
					 addWorkList(!u))
			else 
					unionInSet(activeMoves,singletonSet Int.compare m))  
	end
	
fun freezeMoves u = 
	let
		fun aux m =
			let
				val instr = List.nth (!linstr,m)
				val (x,y) = case instr of
											MOVEI {src,dst,...} => (src, dst)
										| _ => (operPP instr;
														raise Fail ("2error interno en coalesce:"^Int.toString(m)^"No es un MOVE"))
				val v = if equalTemp (getAlias y , getAlias u) then getAlias x else getAlias y
			in 
				(differenceInSet(activeMoves,singletonSet Int.compare m);
				 unionInSet(frozenMoves,singletonSet Int.compare m);	
				 (if (isEmptySet(nodeMoves(v)) andalso (findMap(degree,v) < k)) then
					(differenceInSet(freezeWorkList,single v);
					 unionInSet(simplifyWorkList,single v))
					else ()))
			end
	in forallSet aux (nodeMoves u) end

fun freeze () = 
	let
		val u = elemFromSet freezeWorkList
	in
		(differenceInSet(freezeWorkList,single u);
		 unionInSet(simplifyWorkList,single u);
		 freezeMoves(u))
	end

val gen = Random.newgen ()

fun selectSpill () =
let
	(* DEBERIA ELEGIR EN BASE A ALGUNA HEURISTICA *)
(*	val ls = listItemSet spillWorkList
	val l = (List.length ls)-1
	val t = Random.range (0,l) gen 
	val m = List.nth(ls,t) *)
	val m = (* List.hd (List.rev (listItemSet spillWorkList)) *) elemFromSet spillWorkList 
(*	val _ = print("tengo que elegir para hacer spill entre:")
	val _ = print(setPP tempastring spillWorkList)	
(*	val _ = mapPP tempastring Int.toString degree *)
	val _ = print("Elijo :" ^ (tempastring m) ^"\n")  *)
(*	val l = Listsort.sort (fn ((t1,g1),(t2,g2)) => Int.compare (g1,g2)) 
					(List.map (fn x => (x,findMap(degree,x))) (listItemSet (spillWorkList)))

	val m = #1(List.hd (List.rev(l))) *)
in
	(differenceInSet(spillWorkList,single m);
	 unionInSet (simplifyWorkList,single m);
	 freezeMoves (m))
end

fun assignColors () =
let
	fun aux () = 
			if (tigerstack.isempty selectStack) then ()
			else
				let
					val n = pop(selectStack)
					val okList = [2,3,4,5,6,7,8,9,10,11,12,13,14,15] 
(*					val okColors = let val t = newSet Int.compare in addListSet (t,(0 :: (listTo (k-1)))) ; t end *)
					val okColors = let val t = newSet Int.compare in addListSet (t,okList) ; t end 
			  in
					(forallSet (fn w => if (memberSet (unionSet(coloredNodes,precolored),getAlias w)) then
								differenceInSet(okColors,singletonSet Int.compare (findMap(color,(getAlias w)))) else ())
						(findMap(adjList,n));
					(if (isEmptySet okColors) then
						unionInSet(spilledNodes,single n)
					else 
					(unionInSet(coloredNodes,single n);
					 insertMap(color,n,elemFromSet okColors)));
					aux ())
				end
in 
(*	print("Asignando colores:\n");
	print("Stack: " ^ (stackPP tempastring selectStack)^"\n");  *)
	aux();
(*	print("Alias\n");
	mapPP tempastring tempastring alias; 
	print("Coalesced Nodes\n");
	print(setPP tempastring coalescedNodes);
	print("Colored Nodes\n");
	print(setPP tempastring coloredNodes);
	print("Spiled Nodes\n");
	print(setPP tempastring spilledNodes);
	print("Color map\n");
	mapPP tempastring Int.toString color; *)
	forallSet (fn n => (if memberSet(spilledNodes,getAlias n) then ()
											else insertMap(color,n,findMap(color,getAlias n)))) coalescedNodes 
end

fun rewriteProgram () = 
let		
	
	val nTemp = newSet compareTemp

	fun instrDef t (OPER{dst,...}) = List.exists (fn x => equalTemp(t,x)) dst
		| instrDef t (MOVEI{dst,...}) = equalTemp(dst,t)
		| instrDef t (LABELI _) = false

	fun instrUse t (OPER{src,...}) = List.exists (fn x => equalTemp(t,x)) src
		| instrUse t (MOVEI{src,...}) = equalTemp(src,t)
		| instrUse t (LABELI _) = false

	fun changeDst (OPER{assem,dst,src,jump},t,nt) = 
							OPER{assem=assem,dst=(List.map (fn x => if equalTemp(x,t) then nt else x) dst),src=src,jump=jump}
		| changeDst (MOVEI{assem,dst,src},t,nt) = MOVEI{assem=assem,dst=nt,src=src}
		| changeDst _ = raise Fail "Error interno en changeDst"
	
	fun changeSrc (OPER{assem,dst,src,jump},t,nt) = 
							OPER{assem=assem,dst=dst,src=(List.map (fn x => if equalTemp(x,t) then nt else x) src),jump=jump}
		| changeSrc (MOVEI{assem,dst,src},t,nt) = MOVEI{assem=assem,dst=dst,src=nt}
		| changeSrc _ = raise Fail "Error interno en changeSrc"
	

	fun storeGen (nt,acc) = OPER{assem="movq %`s0,"^(itoa acc)^"(%`s1) \n",dst=[],src=[nt,fp],jump=NONE}
	fun fetchGen (nt,acc) = OPER {assem="movq "^(itoa acc)^"(%`s0),%`d0 \n",dst=[nt],src=[fp],jump=NONE}

	fun travInstr t acc [] = []
		| travInstr t acc ((pos,oper)::(l)) =  
					if (instrDef t oper) then  
						let
							val nt = newtemp ()
							val _ = updateMovesSets (pos+1)
							val _ = addElemSet(nTemp,nt)
							val l' = List.map (fn (pos,oper) => (pos+1,oper)) l
						in (pos,(changeDst(oper,t,nt))) :: (pos+1,(storeGen(nt,acc))) :: (travInstr t acc l' ) end
					else if (instrUse t oper) then
						let
							val nt = newtemp ()
							val _ = updateMovesSets (pos)
							val _ = addElemSet(nTemp,nt)
							val l' = List.map (fn (pos,oper) => (pos+1,oper)) l
						in (pos,(fetchGen (nt,acc))) :: (pos+1,(changeSrc(oper,t,nt))) :: (travInstr t acc l') end
					else (pos,oper) :: (travInstr t acc l)


	fun aux (v,li)=
		let 
			val access = case (allocLocal (!frame) true) of
								InFrame off => off
							| _ => raise Fail "error interno en allocLocal"
		in travInstr v access li end
	
	val nodesToSpill = listItemSet spilledNodes

(*
	val _ = print("Haciendo spill para los temporales: " ^ (setPP tempastring spilledNodes) ^"\n")
	val _ = instrPP (!linstr) 
	val _ = printMovesStructs()
*)
	val linstr' = ListPair.zip (0::(listTo((length (!linstr))-1)),(!linstr))
	val prog' = List.foldr aux (linstr') nodesToSpill 

	val (_,prog'') = ListPair.unzip prog'

(*
	val _ = printMovesStructs()
	val _ = instrPP (prog'') 
*)
(*	val _ = print("Los temporales nuevos son:")
	val _ = print (setPP tempastring nTemp)
*)
	val _ = setEmptySet(spilledNodes)
	val _ = setEmptySet(initial)

	val _ = unionInSet(initial,coloredNodes)
	val _ = unionInSet(initial,coalescedNodes)
	val _ = unionInSet(initial,nTemp)
	(*
	val _ = print("Initial es:")
	val _ = print (setPP tempastring initial)
	*)
	val _ = setEmptySet(coloredNodes)
	val _ = setEmptySet(coalescedNodes)

	val _ = linstr := prog''

in () end

fun work () = 
	((if (not (isEmptySet simplifyWorkList)) then simplify ()   
	else if (not (isEmptySet workListMoves)) then coalesce ()
	else if (not (isEmptySet freezeWorkList)) then freeze ()
	else if (not (isEmptySet spillWorkList)) then selectSpill ()
	else ()))
(*	print ("\ndegree:\n"); mapPP tempastring Int.toString degree;
	print("\nGrafo de adjacencia:\n" ^ (setPP graphPP adjSet ))) *)
	

fun iter () = 
	(work ();
	if ((isEmptySet simplifyWorkList) andalso (isEmptySet workListMoves) 
			andalso (isEmptySet freezeWorkList) andalso (isEmptySet spillWorkList)) then ()
	else iter ())

fun removeCoalescedMoves () =
	let
		fun aux [] = []
			| aux ((MOVEI {assem,dst,src})::l) = if equalTemp (dst,src) then aux l
																				else (MOVEI {assem=assem,dst=dst,src=src}) :: (aux l)
			| aux (a::l) = a :: (aux l)

	in linstr := (aux (!linstr)) end
	
fun loop () = 
	(
(*	 print("----------------------------------\n");
	 List.app operPP (!linstr); *)
	 build ();
(*	 degreeInvariant (); *)
	 makeWorkList ();
(*	 degreeInvariant (); *)
	(* printWorkList(); *)
(*	 print ("\nSimplifyWorkList:\n"); print (setPP tempastring simplifyWorkList);
	 print ("\nspillWorkList:\n"); print (setPP tempastring spillWorkList);
	 print ("\nfreezeWorkList:\n"); print (setPP tempastring freezeWorkList); *)
	 iter ();
	
	 assignColors ();
	 if (not (isEmptySet spilledNodes)) then (rewriteProgram ();loop())
	 else ())

fun alfaConv () =
	let
		fun tempFromInt n = List.nth(registerList,n)
		fun trTemp t = tempFromInt (findMap (color,t))
		fun f lt = List.map trTemp lt

		fun aux (OPER {assem,dst,src,jump}) = OPER {assem=assem,dst=f dst,src=f src,jump=jump}
			| aux (MOVEI {assem,dst,src}) = MOVEI{assem=assem,dst=trTemp dst, src=trTemp src}
			| aux a = a
	 
	in linstr := (List.map aux (!linstr)) end	

fun elimJumpCons () =
	let
			fun aux ((l1 as OPER {jump=SOME [l],...})::(l2 as LABELI{lab,...})::ls) = 
								if (labelastring l) = (labelastring lab) then l2 :: (aux ls)
								else l1::l2::(aux ls)
				| aux (x::xs) = x :: (aux xs)
				| aux [] = []
	in linstr := aux (!linstr) end	 

in 
	(loop (); 
	 alfaConv (); 
	 removeCoalescedMoves();  
	 elimJumpCons ();
	 procEntryExit3(!frame,!linstr)) 
end

end   
