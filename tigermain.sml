open tigerlex
open tigergrm
open BasicIO Nonstdio
open tigerpp
open tigerescap
open tigerseman
open tigertranslate
open tigercanon
open tigerassem

open tigerliv
open tigercoloring

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")
fun main(args) =
	let	fun arg(l, s) = (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
	in	
		if escapes then findEscape expr else (); 
		if arbol then (print"AST:\n"; exprAst expr) else ();
		if ir then (transProg expr;()) else ();
		if canon then canoFrags (!lfrags) else ();
		if code then 
			let 
				val l = genera (!canonfrags) 
				fun f {frag=CPROC(ci,f),code=SOME e} = ((*printFun(f,e);*)printFun(f,coloreo (e,f)))
					| f {frag=CSTRING(l,s),code} = printString(l,s)
					| f _ = ()
			in List.app f l end 
		else ()
	
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
