structure tigerassem :> tigerassem =
struct

open tigertree
open tigertemp
open tigerutils
open tigerframe

type reg = string
	
datatype instr = OPER of { assem: string,
													 dst: tigertemp.temp list,
													 src: tigertemp.temp list,	
													 jump: tigertemp.label list option }
								| LABELI of { assem: string,
														  lab: tigertemp.label } 
								| MOVEI of { assem: string,
													 	 dst: tigertemp.temp,
													 	 src: tigertemp.temp }


fun isMoveI (MOVEI _) = true
	| isMoveI _ = false

fun isJump (OPER{jump=SOME _,...}) = true
	| isJump _ = false

fun codegen (stm,frame) = 
let
	val ilist = ref (nil: instr list)
	
	fun emit x = ilist := x :: !ilist

	fun result gen = let val t = newtemp() in gen t; t end

	fun	munchexp (CONST i) = 
		result (fn r => emit (OPER {assem = "movq $"^(itoa i)^",%`d0 \n",
												 			   dst = [r], src = [],jump = NONE } ))
		
	| munchexp (TEMP t) = t
	
	| munchexp (MEM e) = 
		result (fn r => emit (OPER {assem = "movq (%`s0),%`d0 \n",
																 dst = [r], src = [munchexp e],jump=NONE } ))
	
	| munchexp (BINOP (PLUS, CONST j, CONST i)) =
		result (fn r => (emit (OPER {assem = "movq $"^(itoa(j+i))^",%`d0 \n",dst=[r],src=[],jump=NONE})))

	| munchexp (BINOP (PLUS, e, CONST i)) =
		result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=munchexp e});
										 emit (OPER {assem = "addq $"^(itoa i)^",%`d0 \n",dst=[r],src=[r],jump=NONE})))

	| munchexp (BINOP (PLUS,e1,e2)) = 
		result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=munchexp e1});
										 emit (OPER {assem = "addq %`s0,%`d0 \n",dst=[r],src=[munchexp e2,r],jump=NONE})))

	| munchexp (BINOP (MINUS,e1,e2)) =
		result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=munchexp e1});
										 emit (OPER {assem = "subq %`s0,%`d0 \n",dst=[r],src=[munchexp e2,r],jump=NONE})))

	| munchexp (BINOP (MUL,e1,e2)) =
		result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=munchexp e1});
										 emit (OPER {assem = "imulq %`s0,%`d0 \n",dst=[r],src=[munchexp e2,r],jump=NONE})))

	| munchexp (BINOP (DIV,e1,e2)) = 
		let 
			val (mexp1,mexp2) = (munchexp e1, munchexp e2)
		in
			result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=rv,src=mexp1});
											 emit (OPER {assem = "cqto \n",dst=[rv,ov],src=[rv,ov],jump=NONE});
											 emit (OPER {assem = "idivq %`s0 \n",dst=[rv,ov],src=[mexp2,rv,ov],jump=NONE});
							 				 emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=rv}) ))
		end

	| munchexp (BINOP (AND,e1,e2)) = 
		result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=munchexp e1});
										 emit (OPER {assem = "andq %`s0,%`d0 \n",dst=[r],src=[munchexp e2,r],jump=NONE})))

	| munchexp (BINOP (OR,e1,e2)) = 
		result (fn r => (emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=r,src=munchexp e1});
										 emit (OPER {assem = "orq %`s0,%`d0 \n",dst=[r],src=[munchexp e2,r],jump=NONE})))

	| munchexp (NAME l) = 
		result (fn r => (emit (OPER {assem = "movq $"^(labelastring l)^",%`d0 \n",dst=[r],src=[],jump=NONE})))

	| munchexp (ESEQ (s,e)) =  (munchstm s ; munchexp e)

	| munchexp _ = raise Fail "munchExp incompleto"


	and munchstm (SEQ (s1,s2)) = (munchstm s1; munchstm s2)

	| munchstm (MOVE (MEM e1, MEM e2)) = 
			let
				val t = newtemp()
				val (e1',e2') = (munchexp e1, munchexp e2)
			in
				emit (OPER {assem = "movq (%`s0),%`d0 \n",dst=[t],src=[e2'],jump=NONE});
				emit (OPER {assem = "movq %`s0,(%`s1) \n",dst=[],src=[t,e1'],jump=NONE})
			end

	| munchstm (MOVE(TEMP t1, MEM(BINOP(PLUS, TEMP t2, CONST i)))) =
				emit (OPER {assem = "movq "^(itoa i)^"(%`s0),%`d0 \n", dst=[t1], src=[t2], jump=NONE})

	| munchstm (MOVE(MEM(BINOP(PLUS, e,CONST i)), CONST j)) = 
				emit (OPER {assem = "movq $"^(itoa j)^","^(itoa i)^"(%`s0) \n",dst=[],src=[munchexp e],jump=NONE})
	
	| munchstm (MOVE(MEM(BINOP(PLUS, e1,CONST i)), e2)) = 
				emit (OPER {assem = "movq %`s1,"^(itoa i)^"(%`s0) \n",dst=[],src=[munchexp e1,munchexp e2], jump=NONE})

	| munchstm (MOVE (MEM e, CONST j)) =
				emit (OPER {assem = "movq $"^(itoa j)^",(%`s0) \n",dst=[],src=[munchexp e],jump=NONE})

	| munchstm (MOVE (MEM e1, e2)) = 
				emit (OPER {assem = "movq %`s0,(%`s1) \n",dst=[],src=[munchexp e2, munchexp e1],jump=NONE})

	| munchstm (MOVE(TEMP t, CONST j)) =
				emit (OPER {assem = "movq $"^(itoa j)^",%`d0 \n",dst=[t],src=[],jump=NONE})

	| munchstm (MOVE(TEMP t, NAME l)) =
				emit (OPER {assem = "movq $"^(labelastring l)^",%`d0 \n",dst=[t],src=[],jump=NONE})

	| munchstm (MOVE (TEMP j, e)) = 
				emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=j,src=munchexp e})

	| munchstm (MOVE(e1,e2)) =
			let val t = newtemp ()
					val (re1,re2) = (munchexp e1,munchexp e2)
			in
				emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=t,src=re2 });
				emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=re1,src=t })
			end


	| munchstm (JUMP (NAME f,l)) = 
				 emit (OPER {assem = "jmp "^(labelastring f)^" \n",dst=[],src=[],jump=SOME l})

	| munchstm (CJUMP (rel,e1,e2,lt,lf)) = 
			let
				val (re1,re2) = (munchexp e1,munchexp e2)
				fun trel r = case r of
									EQ => "je "
								| NE => "jne "
								| LT => "jnae "
								| GT => "ja "	
								| LE => "jbe " 
								| GE => "jae "
								| _ => raise Fail "muchstm incompleto en funcion trel"
			in
				(emit (OPER {assem = "cmpq %`s0, %`s1 \n",dst=[],src=[re2,re1],jump=NONE});
				 emit (OPER {assem = (trel rel)^(labelastring lt)^"\n",dst=[],src=[],jump=SOME [lt]}))
			end

	| munchstm (LABEL l) = emit (LABELI {assem = (labelastring l)^":\n",lab=l})
	

	| munchstm (EXP (CALL (NAME n, args))) = if List.length args<>0 then
		 (emit (OPER {assem = "subq $"^(itoa ((List.length args)*wSz))^",%`d0 \n",dst=[sp],src=[sp],jump=NONE});
			emit (OPER {assem = "call "^(labelastring n)^"\n",dst=callersaves,src=munchargs(0, args),jump=NONE});
			emit (OPER {assem = "addq $"^(itoa ((List.length args)*wSz))^",%`d0 \n",dst=[sp],src=[sp],jump=NONE}))
		else
			emit (OPER {assem = "call "^(labelastring n)^"\n",dst=callersaves,src=munchargs(0, args),jump=NONE})
			

	| munchstm (EXP e) = (munchexp e; ())(* emit (MOVEI {assem = "movq %`s0,%`d0 \n",dst=rv,src=munchexp e}) *)

	| munchstm _ = raise Fail "munchStm incompleto"

and munchargs (_,[]) = []

	| munchargs (i,h::t) = 
		if i<6 then	
			let	
				val d = List.nth(argregs,i)
				val (instr, e) =case h of
					CONST c => (OPER{assem="movq $"^itoa(c)^",%`d0 \n",
												src=[], dst=[d], jump=NONE}, [d] )
(*					| NAME n => (OPER{assem="pushq "^(labelastring n)^"\n",
											src=[], dst=[], jump=NONE}, "") *)
				 |	TEMP n => (MOVEI{assem="movq %`s0,%`d0\n",src=n, dst=d}, [d])
(*					| MEM(TEMP n) => (OPER{assem="pushq (%`s0)\n",
														src=[n], dst=[], jump=NONE}, "")
					| MEM(BINOP(PLUS, TEMP n, CONST c)) => (OPER{assem="pushq "^(itoa c)^"(%`s0)\n",
																									src=[n], dst=[], jump=NONE}, "")  *)
					| _ =>	let	
										val e = munchexp h
									in	
										(MOVEI{assem="movq %`s0,%`d0\n", src=e,dst=d}, [d,e]) 
									end
			in	
				emit(instr);
				if List.null e then munchargs(i+1,t) else  e @ munchargs(i+1, t) 
			end
		else
			let	
				val (instr, e) =case h of
					CONST c => (OPER{assem="movq $"^itoa(c)^","^(itoa (i*wSz))^"(%`s0) \n",
												src=[sp], dst=[], jump=NONE}, [])
(*					| NAME n => (OPER{assem="pushq "^(labelastring n)^"\n",
											src=[], dst=[], jump=NONE}, "") *)
			(*	 		TEMP n => (MOVEI{assem="movq `s0,`d0\n",src=n, dst=(List.nth(argregs,i))}, "") *)
(*					| MEM(TEMP n) => (OPER{assem="pushq (%`s0)\n",
														src=[n], dst=[], jump=NONE}, "")
					| MEM(BINOP(PLUS, TEMP n, CONST c)) => (OPER{assem="pushq "^(itoa c)^"(%`s0)\n",
																									src=[n], dst=[], jump=NONE}, "")  *)
					 |_ =>	let	
									val e = munchexp h
								in	(OPER{assem="movq %`s0,"^(itoa (i*wSz))^"(%`s1) \n", 
													src=[e,sp],dst=[], jump=NONE}, [e]) end
			in	
				emit(instr);
				if List.null e then munchargs(i+1,t) else e @ munchargs(i+1, t)
		end


in munchstm stm ;  (List.rev (!ilist)) end

val p = ref 0
fun inicial() = p := 0
fun inc() = Int.toString (!p) before (p:= !p +1)

fun format saytemp =
let 
	fun speak(assem,dst,src,jump) =
		let val saylab = tigertemp.labelastring
				fun f(#"`":: #"s":: i::rest) = 
		    		(explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
		  		| f( #"`":: #"d":: i:: rest) = 
		    		(explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
		  		| f( #"`":: #"j":: i:: rest) = 
				    (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
				  | f( #"`":: #"`":: rest) = #"`" :: f rest
				  | f( #"`":: _ :: rest) = raise Fail "bad Assem format"
				  | f(c :: rest) = (c :: f rest)
				  | f nil = nil
    in implode(f(List.rev(List.tl(List.rev (explode assem)))))end
	fun pTL l = "{"^ (String.concat(List.map(fn x => (tempastring x ^ ",")) l)) ^ "}"
in 
fn OPER{assem,dst,src,jump=NONE} =>  (*"OPER\t" ^ speak(assem,dst,src,nil)^"\t\tusa:"^(pTL src)^"\tdef:"^(pTL dst)^"\n" *) "\t" ^ speak (assem,dst,src,nil) ^ "\n" 
 | OPER{assem,dst,src,jump=SOME j} => (*"OPER\t" ^ speak(assem,dst,src,j)^"\t\tusa:"^(pTL src)^"\tdef:"^(pTL dst)^"\n" *) "\t" ^ speak (assem,dst,src,nil) ^ "\n" 
 | LABELI{assem,...} => (* "LABEL\t" ^ assem *) assem (*^ "\n" *)
 | MOVEI{assem,dst,src} =>  (*"MOVE\t" ^ speak(assem,[dst],[src],nil) ^ "\t\tusa:"^(pTL [src])^"\tdef:"^(pTL [dst])^"\n"*) "\t" ^ speak(assem,[dst],[src],nil) ^ "\n" 
end

fun operPP oper = print (format tempastring oper)
fun instrPP linstr = (inicial() ; List.app (fn oper => (print(inc()^":");operPP oper))) linstr

fun procEntryExit2 (frame,body) =
			body @ [OPER{assem="\n",src=[rv]@calleesaves,dst=[],jump=NONE}]


fun procEntryExit3 (frame,body) =
	let
		val cantLocales = actualLocalVar frame
		val prologue = [ OPER {assem="pushq %`s0 \n",src=[fp],dst=[],jump=NONE},
										 OPER {assem="movq %`s0,%`d0 \n",src=[sp],dst=[fp],jump=NONE}]
		val subSpInstr = [OPER {assem="subq $"^(itoa (Int.abs(cantLocales*wSz)))^",%`d0 \n",src=[sp],dst=[sp],jump=NONE}]
		val addSpInstr = [OPER {assem="addq $"^(itoa (Int.abs(cantLocales*wSz)))^",%`d0 \n",src=[sp],dst=[sp],jump=NONE}]
		val epilogue = [ OPER {assem="movq %`s0,%`d0 \n",src=[fp],dst=[sp],jump=NONE},
										 OPER {assem="popq %`d0 \n",src=[],dst=[fp],jump=NONE},
										 OPER {assem="ret \n",src=[],dst=[],jump=NONE}]
	in 
		prologue @ 
		(if cantLocales<>0 then subSpInstr else []) @ 
		body @ 
		(if cantLocales<>0 then addSpInstr else []) @
		epilogue 
	end


fun genera (l) =
let
	fun aux (tigercanon.CPROC (ls,f)) = 
			let 
				val code = List.concat (List.map (fn x => codegen (x,f)) ls)
				val code' = procEntryExit2 (f,code)
			in {frag=tigercanon.CPROC(ls,f),code=SOME code'} end 
			
		| aux	(tigercanon.CSTRING(l,s)) = {frag=tigercanon.CSTRING(l,s),code=NONE} 
in 
	List.map aux l 
end

fun printFun (frame,code) =
	let
				val _ = print ("\n.text\n\n")
				val _ = print(".globl " ^ (labelastring (name frame))^"\n")
				val _ = print (labelastring (name frame)^":\n") 
				val _ = List.app operPP code 
	in () end

fun printString (l,s) = 
	let
		fun stringlen s =
    	let
    		fun aux [] = 0
    			| aux((#"\\")::(#"x")::_::_::t) = 1 + aux t
    			| aux(_::t) = 1 + aux t
    	in aux (explode s) end
		val _ = print ("\t.data\n");
		val _ = print ((labelastring l)^":\n")
		val _ = print ("\t.quad "^ Int.toString(stringlen s)^"\n")
		val _ = print ("\t.string \"" ^ s ^ "\"\n")
	in () end
	
end
