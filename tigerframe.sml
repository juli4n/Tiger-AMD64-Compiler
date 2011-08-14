(*
	Frames para AMD64 .
		
		|		 arg n   |  fp+16+wSz*n
		|		 ...		 |
		|		 arg7    |  fp+72
		| arg6-space |	fp+64
		|    ...     |
		| arg2-space |	fp+32
		| arg1-space |	fp+24
		|	static-link|  fp+16
		|  retorno   |	fp+8
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-8
		|   local2   |	fp-16
		|    ...     |
		|   localn   |	fp-wSz*n

Nota1: Se reserva espacio en el frame para los primeros 6 argumentos, si
el callee necesita mandarlos a memoria, usa este espacio.

Nota2: fp+16 corresponde al "static link". Este contiene la direccion
base del stack del caller.

*)

structure tigerframe :> tigerframe = struct

open tigertree
open tigerutils

type level = int

val fp = namedtemp "rbp"				(* frame pointer *)
val sp = namedtemp "rsp"				(* stack pointer *)
val rv = namedtemp "rax"				(* return value  *)
val rbx = namedtemp "rbx"
val rcx = namedtemp "rcx"
val ov = namedtemp "rdx"				(* overflow value (edx en el 386) *) 
val rdi = namedtemp "rdi"
val rsi = namedtemp "rsi"
val r8 = namedtemp "r8"
val r9 = namedtemp "r9"
val r10 = namedtemp "r10"
val r11 = namedtemp "r11"
val r12 = namedtemp "r12"
val r13 = namedtemp "r13"
val r14 = namedtemp "r14"
val r15 = namedtemp "r15"

val precoloredList = [fp,sp,rv,rdi,rsi,ov,rcx,r8,r9]
val registerList = [fp,sp,rv,rbx,rdi,rsi,ov,rcx,r8,r9,r10,r11,r12,r13,r14,r15]

val wSz = 8											(* word size in bytes *)
val log2wSz = 3									(* base two logarithm of word size in bytes *)
val fpPrev = 0									(* offset (bytes) *)
val fpPrevLev = 2*wSz						(* offset (bytes) *)
val argsInicial = 1							(* ordinal *)
val argsOffInicial = 2*wSz			(* bytes *)
val argsDelta = wSz							(* bytes *)
val argsInc = 1
val localsInicial = 0						(* ordinal *)
val localsOffInicial = 0				(* bytes *)
val localsDelta = ~wSz 					(* bytes *)
val localsInc = ~1
val calldefs = [rv,ov]
val specialregs = [rv, fp, sp]
val callersaves = [rv,rdi,rsi,ov,rcx,r8,r9,r10,r11]
val calleesaves = [fp,sp,rbx,r12,r13,r14,r15]
val movesIntroExitRegs = [rbx,r12,r13,r14,r15]

val argregs = [rdi,rsi,ov,rcx,r8,r9]
val argsinreg = length (argregs)

datatype access = InFrame of level | InReg of tigertemp.temp

type frame = {
	name: tigertemp.label,
	formals: bool list,
	locals: bool list,
	actualArg: int ref,      
	actualLocal: int ref,
	accessFormals : access list ref,
	movs : tigertree.stm list ref
}

fun actualLocalVar (f:frame) = !(#actualLocal f)

datatype frag = PROC of tigertree.stm * frame
							| STRING of tigertemp.label * string

type register = string

val slaccess = InFrame (fpPrevLev)
val slmov = MOVE(MEM(BINOP(PLUS,TEMP fp,CONST fpPrevLev)),TEMP (List.hd argregs))

fun newFrame{name, formals} = {
	name= name,
	formals=formals,
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	accessFormals = ref [slaccess], (* Siempre el primero es el static link *)
	movs = ref [slmov]
}

fun name(f: frame) = #name f

fun string(s, l) = l^tigertemp.labelastring(s)^"\n"


fun formals (f:frame) = !(#accessFormals f)
 

fun allocArg (f: frame) =
	let
		val escapa = List.nth (#formals f,!(#actualArg f)) orelse !(#actualArg f)>=6
		val argno = !(#actualArg f)

		val (ret,stm) =  case escapa of
			true => 
				let 		
					val r = (argno*wSz)+argsOffInicial
					val s = if (argno>=6) then [] 
									else  [MOVE (MEM(BINOP(PLUS,TEMP fp,CONST r)),TEMP(List.nth(argregs,argno)))]
				in (InFrame r,s) end
		
		| false =>
				let 
					val r = tigertemp.newtemp()
					val s = [MOVE(TEMP r,TEMP(List.nth(argregs,argno)))]
				in (InReg r,s) end
		
		val _ = #movs f := !(#movs f) @ stm
		val _ = #accessFormals f := !(#accessFormals f) @ [ret]
		val _ = (#actualArg f) := argno + argsInc

	in ret end


fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)*wSz+localsDelta)
			val _ = #actualLocal f:=(!(#actualLocal f)+localsInc)
		in	ret end
	| false => InReg(tigertemp.newtemp())


fun procEntryExit1(f:frame, e) = 
	let
		val movs = seq (!(#movs f))

		fun entryExitMoves e = 
			let val t = newtemp ()
			in (MOVE(TEMP t,TEMP e),MOVE(TEMP e,TEMP t))  end
	
		val (entry,exit) = ListPair.unzip (List.map entryExitMoves movesIntroExitRegs)

	in seq (entry @ [movs,e] @ exit) end

end
