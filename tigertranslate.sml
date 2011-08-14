structure tigertranslate =
struct

open tigertree
open tigerabs
open tigertemp
open tigerit
open tigerframe
open tigerstack
open tigerutils

datatype exp =  Ex of tigertree.exp
							| Nx of tigertree.stm
							| Cx of tigertemp.label * tigertemp.label -> tigertree.stm

type level = {parent:frame option , frame: frame, level: int,stack: label stack}
type access = tigerframe.access
type frag = tigerframe.frag

val lfrags = ref []

fun addfrag f = lfrags := f::(!lfrags)

fun functionname n = labelastring (newlabel())^n

fun printFragList () = 
	let
		fun f (PROC (body,frame)) = print ("\n\t" ^ (labelastring (name frame)) ^ ":\n" ^ (tigerit.tree body))
			| f (STRING(l,str)) = print ((labelastring l) ^ ":\n" ^ str)
	in
		List.app f (!lfrags)
	end

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

fun incActualLev() = actualLevel := !actualLevel + 1
fun decActualLev() = actualLevel := !actualLevel - 1

fun levelframe {frame,parent,level,stack} = frame
fun levelstack {frame,parent,level,stack} = stack


val outermost: level = {
	parent=NONE,
	frame=newFrame{name=namedlabel("_tigermain"), formals=[]},
	level=0,
	stack = newstack () }

val lstack = newstack()
val _ = push lstack outermost

fun gettoplevel () =  top lstack

fun inLevel s = push lstack s

fun outLevel () = pop lstack

fun newlevel (name,formals) = {
	parent = SOME (#frame (top lstack)),
	frame = newFrame {name = name, formals=formals},
	level = getActualLev(),
	stack = newstack ()
}

fun allocArg l = 
					let 
						val f = levelframe l
					in	tigerframe.allocArg f end

fun allocLocal l b = 
					let 
						val f = levelframe l 
					in tigerframe.allocLocal f b end


fun formals {parent, frame, level} = tigerframe.formals frame


fun preWhileFor () = push (levelstack (gettoplevel())) (newlabel ())

fun postWhileFor () = pop (levelstack (gettoplevel()))


fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s,CONST 0)
	| unEx (Cx c) = 
				let val r = newtemp()
						val (t,f) = (newlabel(),newlabel())
				in 
					ESEQ( seq [ MOVE (TEMP r , CONST 1),
											c(t,f),
											LABEL f,
											MOVE (TEMP r, CONST 0),
											LABEL t] , TEMP r)
				end
											
fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx c) = EXP (unEx (Cx c))

fun unCx (Ex (CONST 0)) (t,f) = JUMP(NAME f,[f])
	|	unCx (Ex (CONST _)) (t,f) = JUMP(NAME t,[t]) 
	| unCx (Ex e) (t,f) = CJUMP (NE,e,CONST 0,t,f)
	| unCx (Cx c) (t,f) = c(t,f)
	| unCx _ _ = raise Fail "error interno translate #3"		

fun itPP x = print (tigerit.tree (unNx x))

fun intexp n = Ex(CONST(n))

fun unitexp () = Nx(EXP(CONST(0)))

fun nilexp () = Ex(CONST(0))

fun ifexp1 (test,then') = 
					let 
							val (t,f) = (newlabel(),newlabel())
					in
						Nx (seq [ unCx test (t,f),
									LABEL t,
									unNx then',
									LABEL f ])
					end

fun ifexp2 (test,then',else') =
					let
							val (t,f) = (newlabel(),newlabel())
							val fin = newlabel()
							val r = newtemp()
					in
						Ex (ESEQ ( (seq [ unCx test (t,f),
								              LABEL t,
															MOVE (TEMP r, unEx then'), 
								              JUMP (NAME fin,[fin]),
								              LABEL f,
															MOVE (TEMP r, unEx else'),
															LABEL fin]) , TEMP r))
					end

fun stringexp s = 
			let
				val l = newlabel ()
				val _ = addfrag (STRING (l,s))
			in 
				Ex(NAME l) 
			end

fun compstring (s1,s2,oper) =
		let
			val (t1,t2,tr) = (newtemp(),newtemp(),newtemp())
			val opc = case oper of
								EqOp => 0
							| NeqOp => 1
							| LtOp =>  2
							| LeOp =>  3
							| GtOp =>  4
							| GeOp =>  5
						 	| _ => raise Fail "Error interno en compstring1"
		in
			Ex (ESEQ ( seq [ MOVE (TEMP t1, unEx s1),
					 			 			 MOVE (TEMP t2, unEx s2),
								 			 EXP (CALL (NAME (namedlabel "_compString"),[CONST opc,TEMP t1, TEMP t2])),
								 			 MOVE (TEMP tr, TEMP rv)] ,
				 			 TEMP tr))
		end		


fun operarit(oper,expl,expr) =
      let
        fun trOp PlusOp = PLUS
          | trOp MinusOp = MINUS
          | trOp TimesOp = MUL
          | trOp DivideOp = DIV
					| trOp _ = raise Fail "Error interno en translate-operarit"

				fun operTr PlusOp (a,b) = a+b
          | operTr MinusOp (a,b) = (a-b)
          | operTr TimesOp (a,b) = (a*b)
          | operTr DivideOp (a,b) = a div b
					| operTr _ _ = raise Fail "Error interno en translate-operarit"

        val tr = newtemp()

        fun aux (CONST a,CONST b) = Ex (CONST (operTr oper (a,b)))

          | aux (a, CONST b) = let val t1 = newtemp() in
                                Ex (ESEQ (seq [ MOVE (TEMP t1, a),
                                                MOVE (TEMP tr, BINOP (trOp oper, TEMP t1, CONST b))],TEMP tr))
                               end

          | aux (CONST a,b) = let val t1 = newtemp () in
                                Ex (ESEQ(seq [ MOVE (TEMP t1, b),
                                      MOVE (TEMP tr, BINOP (trOp oper,CONST a,TEMP t1))],TEMP tr))
                               end

          | aux (a,b) = let val (t1,t2) = (newtemp(),newtemp()) in
                        Ex (ESEQ (seq [ MOVE (TEMP t1, a),
                                        MOVE (TEMP t2, b),
                                        MOVE (TEMP tr, BINOP (trOp oper, TEMP t1, TEMP t2))],TEMP tr))
                        end

      in aux(unEx expl,unEx expr) end


fun opercomp1 (oper,expl,expr) =
	let
		fun trRel EqOp = EQ
			| trRel NeqOp = NE 
			| trRel LtOp = LT
			| trRel LeOp = LE
			| trRel GtOp = GT
			| trRel GeOp = GE
			| trRel _ = raise Fail "Error interno en opercomp1"
	
		val r = newtemp()
		val (t,f) = (newlabel(),newlabel())
	in
		Ex (ESEQ ( seq [ MOVE (TEMP r, CONST 0),
									 	 CJUMP(trRel oper,unEx expl, unEx expr,t,f),
							   		 LABEL(t),
								 		 MOVE (TEMP r, CONST 1),
								 		 LABEL (f) ] , TEMP r ))
	end

fun seqexp s = 
				let val r = List.map unNx (init s)
					  val u = unEx (List.last s)
				in Ex (ESEQ(seq r,u)) end

fun assignexp (rvexp,vexp) = Nx (MOVE(unEx vexp,unEx rvexp))

fun whileexp (test,body) = 
					let
							val s = levelstack (gettoplevel	())
							val (t,f) = (newlabel(),top s)
							val i = newlabel ()
					in
							Nx (seq [ LABEL i,
										unCx test (t,f),
										LABEL t,
										unNx body,
										JUMP (NAME i,[i]),
										LABEL f ])
					end

fun breakexp () = 
	let 
		val s = levelstack (gettoplevel	())
	in
		Nx (JUMP( NAME(top s) , [top s] )) 
	end

fun forexp (lo,hi,body,var) = 
      let
					val s = levelstack (gettoplevel	())
					val max = newtemp()
					val v = unEx var
 					val (l1,l2) = (newlabel(),top s)
			in
					Nx (seq [ MOVE (v, unEx lo),
										MOVE (TEMP max, unEx hi),
										CJUMP (GT,v, TEMP max,l2,l1),
										LABEL l1,
										unNx body,
										MOVE (v, BINOP(PLUS,v, CONST 1)),
										CJUMP (GT,v,TEMP max,l2,l1),
										LABEL l2 ])
			end

fun flink t = MOVE (TEMP t, MEM (BINOP(PLUS, TEMP fp, CONST fpPrevLev)))
fun slink t = MOVE (TEMP t, MEM (BINOP (PLUS, TEMP t, CONST fpPrevLev )))

fun simplevar (acc,level) = 
			case acc of
					InReg t => Ex (TEMP t)
				| InFrame off =>  ( case (level-getActualLev()) of
						0 => Ex (MEM (BINOP (PLUS, TEMP fp, CONST off)))
						| l => let val t = newtemp () 
										 	val sl = flink t :: (listn (slink t) (Int.abs (l)-1) )
								 	 	in 
									 		Ex (ESEQ ( seq sl , MEM (BINOP (PLUS,TEMP t, CONST off))))
								 		end )

fun subscriptvar (v,i) =
	let
		val t1 = newtemp ()
		val t2 = newtemp ()
	in
		Ex (ESEQ ( seq [ MOVE (TEMP t1, unEx v),
										 MOVE (TEMP t2, unEx i),
										 EXP (CALL (NAME (namedlabel "_checkIndex"),[TEMP t1, TEMP t2]))] ,
							 MEM (BINOP (PLUS, BINOP (MUL,TEMP t2, CONST wSz), TEMP t1))))
	end

fun fieldvar (r,off) = 
	let	
		val t1 = newtemp ()
	in
		Ex (ESEQ ( seq [ MOVE (TEMP t1, unEx r),
										 EXP (CALL (NAME (namedlabel "_checkNil"),[TEMP t1]))],
							 MEM ( BINOP (PLUS, TEMP t1, CONST(off*wSz)))))

	end

fun recordexp lexp =
	let
		val tr = newtemp ()
		fun aux (e,i) = if i=0 then MOVE (MEM(TEMP tr),unEx e)
										else 	MOVE (MEM(BINOP(PLUS,TEMP tr,CONST(i*wSz))),unEx e)
	in
		Ex (ESEQ (seq ([MOVE (TEMP tr, ESEQ(EXP(CALL(NAME (namedlabel "_newRecord"),[CONST(length lexp)])),
									TEMP rv))] @ (List.map aux lexp)), TEMP tr))
	end

fun arrayexp (size,init) =
	let
		val (ts,ti,tr) = (newtemp(),newtemp(),newtemp())
	in
		Ex ( ESEQ ( seq [ MOVE (TEMP ts, unEx size),
											MOVE (TEMP ti, unEx init),
											EXP (CALL ((NAME (namedlabel "_newArray"),[TEMP ts, TEMP ti]))),
											MOVE (TEMP tr, TEMP rv) ] , TEMP tr))
	end


fun callexp (fname,largs,isproc,level,extern) =
	let
		fun aux e = case (unEx e) of
				(CONST i) => (CONST i)
			| a => 	let 
								val t = newtemp() 
							in ESEQ(MOVE(TEMP t , unEx e),TEMP t) end

		val t = newtemp ()

		val sl = case (level - getActualLev()) of
					0 => MEM (BINOP (PLUS, TEMP fp, CONST fpPrevLev))
				| 1 => TEMP fp
				| l => ESEQ ( seq (flink t :: (listn (slink t) (Int.abs (l)-1))), TEMP t)

		val tr = newtemp ()

	in 
		if extern then 
			if isproc then 		
					Nx ( EXP (CALL (NAME fname, List.map aux largs)))
			else
				Ex (ESEQ ( seq [EXP (CALL (NAME fname, (List.map aux largs))),
										 MOVE (TEMP tr, TEMP rv)] , TEMP tr))
		else
			if isproc then 
					Nx ( EXP (CALL (NAME fname, sl::(List.map aux largs))))
			else
					Ex (ESEQ ( seq [ EXP (CALL (NAME fname, sl::(List.map aux largs))),
										 MOVE (TEMP tr, TEMP rv)] , TEMP tr))
	end

fun vardec (rvexp,vexp) = MOVE (unEx vexp, unEx rvexp)

fun letexp (inits,body) = if List.null inits then Ex (unEx body)														
													else Ex (ESEQ (seq inits,unEx body))


fun functiondec (body,isproc) = 
	let
		val body' = if isproc then (unNx body)
								else (MOVE (TEMP rv,unEx body))
		val frame = #frame(gettoplevel())
		val _ = addfrag (PROC (procEntryExit1 (frame,body'),frame))
	in Nx body' end

end
