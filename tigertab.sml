structure tigertab :> tigertab =
struct
open Polyhash

type ('a, 'b) Tabla = ('a, 'b) hash_table

exception yaExiste of string
exception noExiste
exception noExisteS of string

(* Crea una tabla Hash de tamano 100.						*)
(* La excepcion lanzada en el caso de una busqueda fallida es noExiste 		*)
   
(* tabNueva : (''a,'b') Tabla *)
fun tabNueva() = mkPolyTable(100, noExiste)

fun fromTab t =
	let	val t' = tabNueva()
	in	apply (fn x => insert t' x) t; t' end

(* fun name x = x *)

fun tabEsta(s, t) = case peek t s of
		SOME _ => true
		| NONE => false

fun tabInserta(s, e, t) = (peekInsert t (s, e); copy t)

fun tabRInserta(s, e, t) = (insert t (s, e); copy t)

fun tabBusca(s, t) = peek t s

fun tabSaca(s, t) =
	case tabBusca(s, t) of
	SOME t => t
	| NONE => raise noExiste

fun tabAplica(f, t) = map(fn(_, e) => f e) t

fun tabAAplica(f, g, t) = 
	let	val l' = listItems t
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k, e))
			(List.map(fn(k, e) => (f k, g e)) l');
		t'
	end
	
fun tabRAAplica(f, g, t) = 
	let	val l' = rev(listItems t)
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k, e))
			(List.map(fn(k, e) => (f k, g e)) l');
		t'
	end
	
fun tabInserList(t, l) = (List.app(fn(s, e) => insert t (s, e)) l; t)

fun tabAList t = listItems t

fun tabFiltra(f, t) =
	let	val l = listItems t
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k,e))
			(List.filter (fn(a, b) => f b) l);
		t'
	end

fun tabPrimer(f, t) = hd(List.filter (fn(a, b) => f b) (listItems t))

fun tabClaves t = List.map (fn(x, y) => x) (listItems t)

fun tabPP f g t = let val lab = tabAList t
		  in
		    	String.concat (List.map (fn (a,b) => ("{ " ^ (f a) ^ " -> " ^ (g b) ^ " }\n")) lab)
		  end

end
