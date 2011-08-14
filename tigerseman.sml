structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open topsort
open tigertranslate
open tigerstack
open tigerutils

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(tabNueva(),
	[("int", TInt), 
	("string", TString)
	])

exception typeError of string
exception assertion

fun isNilExp f = case f of
			 (NilExp _) => true
			| _ => false

fun checkTypes (t1,t2) = case (t1,t2) of
			  (TNil,TRecord _) => true
			| (TRecord _,TNil) => true
			| (TTipo (_,a),t2) => (case !a of
														SOME t1 => checkTypes (t1,t2)
														| NONE => raise typeError "error interno.")
			| (t1,TTipo(_,a)) => (case !a of
														SOME t2 => checkTypes (t1,t2)
														| NONE => raise typeError "error interno.")
			| (TArray (_,u1),TArray (_,u2)) => u1=u2
			| (TRecord (_,u1),TRecord (_,u2)) => u1=u2	
			| (a,b) => a=b

fun checkLTypes (l1,l2) = (List.length l1) = (List.length l2)
													andalso ListPair.all checkTypes (l1,l2)

val mainLevel = tigertranslate.outermost

val tab_vars : (string, EnvEntry) Tabla = tabInserList(tabNueva(),
	[("print", Func{level=mainLevel, label= "print",formals=[TString], result=TUnit, extern=true}),
	 ("printint", Func{level=mainLevel, label= "printint",formals=[TInt], result=TUnit, extern=true}),
	 ("randomint", Func{level=mainLevel, label= "randomint",formals=[TInt], result=TInt, extern=true}),
	 ("flush", Func{level=mainLevel, label= "flush",formals=[], result=TUnit, extern=true}),
	 ("getstr", Func{level=mainLevel, label= "getstr",formals=[], result=TString, extern=true}),
	 ("ord", Func{level=mainLevel, label= "ord",formals=[TString], result=TInt, extern=true}),
	 ("chr", Func{level=mainLevel, label= "chr",formals=[TInt], result=TString, extern=true}),
	 ("size", Func{level=mainLevel, label= "size",formals=[TString], result=TInt, extern=true}),
	 ("substring", Func{level=mainLevel, label= "substring",formals=[TString, TInt, TInt], result=TString, extern=true}),
	 ("concat", Func{level=mainLevel, label= "concat",formals=[TString, TString], result=TString, extern=true}),
	 ("not", Func{level=mainLevel, label="not",formals=[TInt], result=TInt, extern=true}),
	 ("exit", Func{level=mainLevel, label="exit",formals=[TInt], result=TUnit, extern=true})
	 ])


fun printTypeError (venv,tenv,name,tesperado,tencontrado,str,nl) =
		let val msgp = "Error de tipos"
		    val msgnl = " en linea: " ^ (Int.toString nl)  ^ "\n"
		    val venvpp = tigertab.tabPP id tigersres.enventryPP venv
		    val tenvpp = tigertab.tabPP id tigertips.tipoPP tenv
			val mensaje = msgp ^ msgnl ^ str ^ "\n" 
		    val _ = print mensaje
			val tesp = case tesperado of
					SOME e => print ("Tipo esperado para " ^ name ^ ": " ^ e ^ "\n")
					| NONE => ()
		    val tenc = case tencontrado of
					SOME e => print ("Tipo encontrado: " ^ e ^ "\n")
					| NONE => ()
		in
		    raise typeError ""
		end
				
fun rname n = case n of
							NameTy s => s
							| _ => raise Fail "Error interno en FunctionDec"

val tpp = tigertips.tipoPP

fun transExp(venv, tenv) =
	let fun trexp(VarExp v) = trvar(v)

		| trexp(UnitExp _) = {exp=unitexp(), ty=TUnit}
		
		| trexp(NilExp _)= {exp=nilexp(), ty=TNil}
		
		| trexp(IntExp(i, _)) = {exp=intexp(i), ty=TInt}
		
		| trexp(StringExp(s, _)) = {exp=stringexp(s), ty=TString}
		
		| trexp(CallExp({func=f, args}, nl)) =
			let 
				fun aux x = let val {exp,ty} = trexp x in (exp,ty) end
        val (eargs',targs') = ListPair.unzip (List.map aux args)	
				val tesp = tpp (TFunc (targs',TUnit))
				val (level,label,targs,ret,extern) =
						(case (tabBusca (f,venv)) of
				 		  SOME (Func {level,label,formals,result,extern}) => (level,label,formals,result,extern)
						| SOME (Var {ty,ro,...}) => printTypeError 
								(venv,tenv,f,SOME tesp,SOME(tpp ty),"CallExp1",nl)
						| NONE => printTypeError (venv,tenv,f,SOME tesp,NONE,"CallExp2",nl))
				val isProc = ret=TUnit
				val leveli = #level level
				val fname = namedlabel label

			in
				if checkLTypes (targs,targs') then {exp=callexp(fname,eargs',isProc,leveli,extern), ty=ret}
				else printTypeError (venv,tenv,f,SOME tesp,SOME(tpp(TFunc(targs,ret))),
						 "CallExp3",nl)
			end
				
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				fun operComp1 (left,oper,right,nl) = 
				let
					val {exp=expl,ty=lty} = trexp left
					val {exp=expr,ty=rty} = trexp right
					fun f (lty,rty) = 
						case (lty,rty) of
					  	(TInt,TInt) => {exp=opercomp1(oper,expl,expr),ty=TInt}
						| (TString,TString) => {exp=compstring(expl,expr,oper),ty=TInt}
						| (TRecord (_,u1),TRecord (_,u2)) => if u1=u2 then 
																{exp=opercomp1(oper,expl,expr),ty=TInt}
														    else raise typeError ""
						| (TArray (_,u1),TArray(_,u2)) => if u1=u2 then 
																{exp=opercomp1(oper,expl,expr),ty=TInt}
														  	else raise typeError ""
						| (TRecord _,TNil) => {exp=opercomp1(oper,expl,expr),ty=TInt}
						| (TNil,TRecord _) => {exp=opercomp1(oper,expl,expr),ty=TInt}
						| (TTipo (_,a),b) => (case !a of
																		  SOME t => f (t,b)
																		| NONE => raise Fail "error interno")
						| (a,TTipo(_,b)) => (case !b of 
																	  SOME t => f (a,t)
																	| NONE => raise Fail "error interno")
						| (a,b) => raise typeError (tpp a ^ tpp b)
				in
					f(lty,rty)
				end

				fun operArit (left,oper,right,nl) =
				let
					val {exp=expl,ty=lty} = trexp left
					val {exp=expr,ty=rty} = trexp right
					val e = operarit(oper,expl,expr)
				in
					case (lty,rty) of
					  (TInt,TInt) => {exp=e,ty=TInt}
					| (a,b) => raise typeError "operaciones aritmeticas esperan enteros"
				end

				fun operComp2 (left,oper,right,nl) = 
				let
					val {exp=expl,ty=lty} = trexp left
					val {exp=expr,ty=rty} = trexp right
				in
					case (lty,rty) of
					  (TInt,TInt) => {exp=opercomp1(oper,expl,expr),ty=TInt}
					| (TString,TString) => {exp=compstring(expl,expr,oper),ty=TInt}
					| (a,b) => raise typeError "Solo se pueden comparar string e int"
				end

			in
				case oper of
					  EqOp => operComp1(left,oper,right,nl)
					| NeqOp => operComp1 (left,oper,right,nl)
					| PlusOp => operArit (left,oper,right,nl)
					| MinusOp => operArit (left,oper,right,nl)
					| TimesOp => operArit (left,oper,right,nl)
					| DivideOp => operArit (left,oper,right,nl)
					| LtOp => operComp2 (left,oper,right,nl)
					| LeOp => operComp2 (left,oper,right,nl)
					| GtOp => operComp2 (left,oper,right,nl)
					| GeOp => operComp2 (left,oper,right,nl)
			end

		| trexp(RecordExp({fields, typ}, nl)) = 
		let 	
				fun f (s,e) = let val {exp,ty} = trexp e
											in (s,exp,ty) end
				val l = List.map f fields

				val lstdec = List.map (fn (s,exp,ty) => (s,ty)) l

				val tesp = tpp (TRecord 
									 ((List.map (fn (s,t) => (s,t,0)) lstdec),ref ()))
				
				val (ltyr,ur) = case (tabBusca(typ,tenv)) of
							  SOME (TRecord (l,u)) => (l,u)
							| SOME t => printTypeError (venv,tenv,"",SOME tesp,
													SOME (tpp t),"RecordExp1",nl)
							| NONE => printTypeError (venv,tenv,"",NONE,NONE,"",nl)

				val lsttabla = map (fn (s,t,_) => (s,t)) ltyr	 

(*				val lsort = Listsort.sort (fn ((a,b),(d,e)) => String.compare (a,d))  
			
				val (ltsn,ltst) = ListPair.unzip (lsort lsttabla) 		
				val (ldsn,ldst) = ListPair.unzip (lsort lstdec)
*)
				val (ltsn,ltst) = ListPair.unzip lsttabla 		
				val (ldsn,ldst) = ListPair.unzip lstdec


				val cond = (ltsn=ldsn) andalso checkLTypes(ltst,ldst)			
	
				val listoff = 0::(listTo ((List.length l)-1))
				val lexp = ListPair.zip (List.map #2 l,listoff)

			in
				if cond then {exp=recordexp(lexp),ty=TRecord(ltyr,ur)}
				else printTypeError
						 (venv,tenv,"",SOME(tpp(TRecord(ltyr,ur))),SOME(tesp),"",nl)
			end 

		| trexp(SeqExp(s, nl)) = 
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=seqexp(exprs), ty=tipo } end
			
		| trexp(AssignExp({var=s, exp}, nl)) =
			let val {exp=rvexp, ty=tyexp} = trexp exp
			    val {exp=vexp, ty=tyvar} = trvar (s,nl)
			    val nv = case s of
			    		(SimpleVar s) => SOME s
					    | _ => NONE
			    val vro = case nv of
			    	(SOME s) => (case tabBusca (s,venv) of
					     					SOME (Var{ty,ro,...}) => ro
					     					| _ => printTypeError 
															(venv,tenv,s,SOME(tpp tyexp),NONE,"",nl ))
					  | NONE => false
			    val namevar = case nv of
			    							SOME s => s 
											| NONE => ""
			in
				if vro then printTypeError 
					(venv,tenv,namevar,NONE,NONE,"Variable read-only.",nl)
				else ();	
				if checkTypes(tyexp,tyvar) then { exp=assignexp(rvexp,vexp),ty=TUnit }
				else printTypeError (venv,tenv,namevar,
														 SOME(tpp tyvar),SOME(tpp tyexp),"",nl)	
			end

		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest, ty=tytest } = trexp test
			    val {exp=expthen', ty=tythen'} = trexp then'
			in
				if tytest = TInt then
					if tythen' = TUnit then {exp=ifexp1(exptest,expthen'),ty=TUnit}
					else printTypeError (venv,tenv,
							"EXP en if () then EXP",SOME "Unit",SOME (tpp tythen'),"",nl)
				else
					printTypeError (venv,tenv,
					"EXP en if (EXP) then EXP2",SOME "Int",SOME(tpp tytest),"",nl)
			end
	
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=exptest, ty=tytest } = trexp test
			    val {exp=expthen, ty=tythen'} = trexp then'
					val {exp=expelse, ty=tyelse'} = trexp else'
			in
			    if tytest = TInt then 
			    	if checkTypes(tythen',tyelse') then 
							{ exp=ifexp2(exptest,expthen,expelse), ty=tythen' }
						else printTypeError (venv,tenv,
								 "EXP2 en if () then EXP1 else EXP2",SOME(tpp tythen'),
							   SOME(tpp tyelse'),"",nl)
					else printTypeError (venv,tenv,
							 "EXP en if EXP then ... else ...",SOME(tpp TInt),
							 SOME(tpp tytest),"",nl)
			end
			
		| trexp(WhileExp({test, body}, nl)) =
			let val {exp=exptest, ty=tytest } = trexp test
					val _ = preWhileFor()
					val {exp=expbody, ty=tybody } = trexp body
			in
			    if tytest=TInt then
					if tybody=TUnit then 
									{ exp=whileexp(exptest,expbody), ty=TUnit }
									before postWhileFor()
					else printTypeError (venv,tenv,
							 "EXP en while ... do EXP",SOME(tpp TUnit),SOME(tpp tybody),"",nl)
				else printTypeError (venv,tenv,
						 "EXP en while EXP do ...",SOME(tpp TInt),SOME(tpp tytest),"",nl)
			end
		
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) = 
				let val {exp=explo,ty=tylo} = trexp lo
				    val {exp=exphi,ty=tyhi} = trexp hi
					val venv' = 
					tabRInserta (var,(Var{ty=TInt,ro=true,level=0,
											access=allocLocal (gettoplevel()) (!escape)}),(fromTab venv))
					val _ = preWhileFor ()
					val {exp=expbody,ty=tybody} = transExp (venv',tenv) body
					val {exp=expv,ty} = transExp (venv',tenv) (VarExp ((SimpleVar var),nl))
				in
					if tybody<>TUnit then 
						printTypeError (venv,tenv,"EXP en for ... do EXP",
						SOME(tpp TUnit),SOME(tpp tybody),"",nl)
					else 
						if tylo=TInt then
							if tyhi=TInt then 
									{exp=forexp(explo,exphi,expbody,expv),ty=TUnit} before postWhileFor()
							else printTypeError (venv,tenv,
									 "EXP2 en for id := EXP1 to EXP2 do ...",
									 SOME(tpp TInt ),SOME(tpp tyhi),"",nl)
						else printTypeError (venv,tenv,
								 "EXP1 en for id := EXP1 to EXP2 do ...",
								 SOME(tpp TInt ),SOME(tpp tylo),"",nl)
				end

		| trexp(LetExp({decs, body}, _)) =
			let 
				fun f ((v,t,ls), d) = 
					let val (v',t',ls')=transDec ((v,t), d)
						in (v',t',ls @ ls') end
				val (venv',tenv',ls) = List.foldl (flip f) (venv,tenv,[]) decs
				val {exp=bodyexp,ty=tybody}=transExp (venv',tenv') body
			in 
				{exp=letexp (ls,bodyexp),ty=tybody}
			end
		
		| trexp(BreakExp nl) = {exp=breakexp(), ty=TUnit}
		
		| trexp(ArrayExp({typ, size, init}, nl)) = 
		let 
			val {exp=exps,ty=typs} = trexp size
			val {exp=expi,ty=typi} = trexp init
			val (tya,u) = case (tabBusca (typ,tenv)) of
					SOME (TArray(t,u)) => (t,u)
					| SOME p => printTypeError (venv,tenv,typ,
											SOME(tpp (TArray (typi,ref ()) ) ),SOME (tpp p),"",nl)
					| NONE => printTypeError (venv,tenv,typ,
										SOME (tpp (TArray (typi,ref ()) ) ),NONE,"",nl)
		in
			if typs <> TInt then printTypeError (venv,tenv,
													 "EXP1 en " ^ typ ^ "[EXP1]:= EXP2",
												   SOME(tpp TInt),SOME(tpp typs),"",nl)
			else if (checkTypes(typi,tya)) then 
			     		{exp=arrayexp(exps,expi),ty=TArray(tya,u)}
					 else
							printTypeError (venv,tenv,"EXP2 en "^typ^"[EXP1]:= EXP2",
							SOME (tpp tya),SOME(tpp typi),"",nl)
		end

			
		and trvar(SimpleVar s, nl) =
			let val t = (case (tabBusca (s,venv)) of 
				  SOME (Var t) => t 
				| SOME (Func _) => printTypeError (venv,tenv,s,NONE,NONE,
													 s^ " es una funcion, se esperaba variable.",nl)
				| NONE => printTypeError (venv,tenv,s,NONE,NONE,"",nl) )
			in 
				{ exp=simplevar(#access t,#level t),ty = #ty t } 
			end
				
		| trvar(FieldVar(v, s), nl) =
		  let 
				val {exp=expvar,ty=tvar} = trvar (v,nl)
				val ls = case tvar of
				 				 TRecord (a,_) => a
								 | TTipo (a,b) => (case !b of
																	 SOME(TRecord (a,_)) => a
																	| SOME a => raise typeError (tpp a)
																	| NONE => raise typeError "error interno")
				 				 | t => printTypeError(venv,tenv,"id en id."^s,
												SOME("Record{ "^s^": ...}"),SOME(tpp t),"",nl)
			in
				case (List.find	(fn l => #1(l)=s) ls) of
				SOME (nombre,tipo,i) => {exp=fieldvar(expvar,i),ty=tipo}
				| NONE => printTypeError (venv,tenv,"",NONE,NONE,
									"No existe campo con nombre " ^ s ^ ".",nl)
			end
			
		| trvar(SubscriptVar(v, e), nl) =
			let 
					val {exp=expv,ty=tvar} = trvar (v,nl)
			    val tipo = case tvar of
								TArray(t,_) => t
								| t => printTypeError (venv,tenv,"id en id[exp]",
									 		 SOME("Array"),SOME(tpp t),"",nl)
					val {exp=expe,ty=texp} = trexp e
			in
				if texp <> TInt then printTypeError (venv,tenv,"exp en A[exp]",
														 SOME(tpp TInt),SOME(tpp texp),"",nl)
				else {exp=subscriptvar(expv,expe),ty=tipo}
			end
	

	and transDec ((venv,tenv),(VarDec({name,escape,typ=NONE,init},nl))) =
			let val {exp=expinit,ty=tye} = transExp (venv,tenv) init
					val acc = allocLocal (gettoplevel()) (!escape)
					val lev = getActualLev()
					val venv'= tabRInserta (name,Var{ty=tye,ro=false,level=lev,
																	access=acc},(fromTab venv))
					val {exp=expv,ty} = transExp (venv',tenv) (VarExp ((SimpleVar name),nl))
			in
				if (isNilExp init) then printTypeError (venv,tenv,"",NONE,NONE,
																"Debe explicitar el tipo de nil.",nl)
				else ( venv' , tenv , [vardec(expinit,expv)])
			end

	| transDec ((venv,tenv),(VarDec({name,escape,typ=SOME t,init},nl))) =
			let val {exp=expinit,ty=tye} = transExp (venv,tenv) init
					val acc = allocLocal (gettoplevel()) (!escape)
					val lev = getActualLev()
				  val vt = case tabBusca(t,tenv) of
						       SOME t => t
						       | NONE => printTypeError (venv,tenv,"",NONE,NONE,
														 "Tipo " ^ t ^ " no declarado.",nl)
					val venv' =  tabRInserta (name,Var{ty=vt,ro=false,level=lev,
												 						access=acc},(fromTab venv))
					val {exp=expv,ty} = transExp (venv',tenv) (VarExp ((SimpleVar name),nl))
			in
				if checkTypes(tye,vt) then ( venv' , tenv , [vardec (expinit,expv)] )
				else printTypeError(venv,tenv,"",NONE,NONE,"No coiciden los tipos.",nl)
			end

	| transDec ((venv,tenv),(FunctionDec lf)) =
			let
				
				val venv' = fromTab venv
						
			 	val _ = incActualLev ()	


				fun insfun ({name,params,result,body},pos) =
				let 
					val tret = case result of
							NONE => TUnit
							| SOME t => (case (tabBusca(t,tenv)) of
										SOME p => p
										| NONE => raise typeError "1")
					
					val pdist = (List.map #name params)
					
					val name' = case name of
								"_tigermain" => name
								| a => functionname a 

(*					val lpn = List.map (fn {name,escape,typ=(NameTy s)} => (name,s)) params  *)
					val lpn = List.map (fn {name,escape,typ} => (name, rname typ)) params  
						
	
					fun f (_,s) = case (tabBusca(s,tenv)) of
								  SOME p => p
								  | NONE => raise typeError "2"
					val lpt = List.map f lpn 		
						
					val bargs =  true :: (List.map (fn p => !(#escape p)) params)

					val nlevel = newlevel (namedlabel name',bargs)

					val ent = Func {formals=lpt,result=tret,extern=false,label=name',
													level=nlevel}

					val _ = tabRInserta ( name,ent,venv') 
				in 
					if (checkDistintos pdist) then () 
					(* Se definio una funcion con dos nombres de argumentos iguales *)
					else raise typeError "8"
				end
				
				val _ = List.app insfun lf
				
				fun g ({name,params,result,body},p) =
					let
(*						val lpn = List.map (fn {name,escape,typ=(NameTy s)} => (name,s)) params *)
						val lpn = List.map (fn {name,escape,typ} => (name,rname typ)) params 
						fun f (n,s) = case (tabBusca(s,tenv)) of
								  SOME p => (n,p)
								  | NONE => raise typeError "3"
						val lpt = List.map f lpn

						val venv'' = fromTab venv'

						val flevel = case tabBusca(name,venv'')  of
								SOME (Func {level,label,formals,result,extern}) => level
							| SOME a => raise Fail "Error interno en FunctionDec 1"
							| NONE => raise Fail  "Error interno en FunctionDec 2"							

						val _ = List.map (fn (a,b) => tabRInserta 
										(a,Var{ty=b,ro=false,access=allocArg flevel,level=getActualLev()},venv'')) lpt
														
						val _ = inLevel flevel 

						val {exp=expfunbody,ty=tyfunbody} = transExp (venv'',tenv) body

						val isProc = (tyfunbody=TUnit)

						val expfunbody' = functiondec(expfunbody,isProc)

						val _ = outLevel ()					

(*						val _ = print ("Cuerpo de funcion ")
						val _ = print name
						val _ = print "\n"
						val _ = itPP expfunbody' *)
						
					in ({exp=expfunbody',ty=tyfunbody},name,p) end

				val ltips = List.map g lf

				val _ = decActualLev ()

				fun f ({exp,ty},n,p) = case (tabBusca (n,venv')) of
									SOME (Func{formals,result,...}) => 
								  if checkTypes(ty,result) then () else raise typeError "4" 
									| SOME _ => raise typeError "5"
									| NONE => raise typeError "6"

				val _ = List.app f ltips

				val lfunname = List.map (fn (a,b) => #name a) lf

	
			in if (checkDistintos lfunname) then (venv',tenv,[]) 
				(* Funciones con el mismo nombre en el batch *)
				else raise typeError "7"
			end

	| transDec((venv,tenv),TypeDec ldt) =
		let
			val tenv' = tabNueva ()
		

			fun tins (n, v, t) =
				case tabBusca(n, t) of
				SOME _ => raise Fail(n^" duplicado!")
				| _ => tabInserta(n, v, t)
			
			fun trdec ({name,ty=(NameTy s)},p) = 
												tins (name, TTipo (s,ref NONE),tenv')
			  | trdec ({name,ty=(ArrayTy s)},p) = 
												tins (name, TArray (TTipo (s,ref NONE), ref ()), tenv')
			  | trdec ({name,ty=(RecordTy l)},p) = 
						let 
						(*	val l' = List.map (fn {name,escape,typ=(NameTy s)} => (name,TTipo (s, ref NONE),0)) l *)
								
(*							  val l' = acumMap (fn ({name,escape,typ},st) => (name, TTipo (rname typ, ref NONE), st))
																 (fn x => x+1) 0 l
*)

								val l' = 	let	
											val c = ref 0
											fun aux {name,escape,typ} = (name,TTipo(rname typ, ref NONE), !c) before (c := !c + 1)
											in List.map aux l end

								val lnc = List.map #name l
						in	 
							if checkDistintos lnc then tins (name, TRecord (l', ref ()),tenv')
							else raise typeError "Campos repetidos en record"
						end
			
			val _ = List.map trdec ldt  
			
			fun trl [] = []
			  | trl ((id1,TTipo (id2,_))::ls) = (id2,id1)::(trl ls)
			  | trl ((id1,TArray(TTipo (id2,_),_))::ls) = (id2,id1)::(trl ls)
			  | trl ((id1,TArray(_,_))::ls) = raise typeError "Err interno typedec #1"
			  | trl ((id1,TRecord(l,u))::ls) =
			  	let 
						val a = trl (List.map (fn (s,t,i) => (id1,t)) l)
					in
						a @ (trl ls)
					end
			  | trl _ = raise typeError "error interno en typedec #2"

			fun filtrar ls = 
						List.filter (fn (id1,id2) => (case tabBusca (id1,tenv') of
														SOME (TRecord _) => false
														| _ => true )) ls
(*
		  val _ = print ("Procesando batch de declaracion de tipos\n")
			val _ = print (tigertab.tabPP id tigertips.tipoPP tenv')
*)
			val ltenv = filtrar (trl (tabAList tenv'))		
			
(*		val _ = print ("Lista para el toposort:\n")
			val _ = List.app (fn (a,b) => print ("( "^a^" , "^b^" )\n")) ltenv
*)	
			val lsorted = topsort ltenv
	
(*		val _ = List.app (fn a => print (a ^ " - ")) lsorted *)
			
			fun busca s = case tabBusca (s,tenv') of
										SOME t => t
									| NONE => ( case tabBusca (s,tenv) of
												  		SOME t => t 
														| NONE => raise typeError ("Tipo no definido " ^ s))
	
			fun reemplaza str b (TTipo (s,r)) = if str=s then b else (TTipo (s,r))
				| reemplaza str b (TArray (t,u)) = TArray ((reemplaza str b t),u)  
				| reemplaza str b (TRecord (ls,u)) =
								let 
								val ls' = List.map (fn (s,t,i) => (s, reemplaza str b t, i)) ls
								in TRecord (ls',u) end
				| reemplaza str b t = t

			fun aplica t f = 
						let val lt = tabAList t
								val _ = List.map (fn (a,b) => tabRInserta (a,f b,t)) lt
						in () end

			val _ = List.map (fn str => aplica tenv' (reemplaza str (busca str))) 
							lsorted

			fun rectypes (TTipo (s,r)) = (case (tabBusca (s,tenv')) of
														  SOME e => (r:=(SOME e);(TTipo (s,r)))
														| NONE => raise typeError "error interno!") 
					| rectypes (TArray (s,u)) = TArray (rectypes s,u)
				  | rectypes (TRecord (ls,u)) =
								let
								val ls' = List.map (fn (s,t,i) => (s,rectypes t,i)) ls
								in TRecord (ls',u) end
					| rectypes a = a

			val _ = aplica tenv' rectypes 

			val tenv'' = fromTab tenv
			val ltenv' = tabAList tenv'
			val _ = List.map (fn (a,b) => tabRInserta (a,b,tenv'')) ltenv'

(*		val _ = print (tigertab.tabPP id tigertips.tipoPP tenv')  *)
	
		in (venv,tenv'',[]) end
		
in trexp end


fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=NONE, body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val {exp,ty} = transExp(tab_vars, tab_tipos) main
	in	exp end

end
