structure tigerstack :>  tigerstack =
struct

type 'a stack = 'a list ref

exception StackVacio

fun newstack () = ref []
fun push s x = (s:=(x::(!s));s)
fun isempty s = List.null (!s)
fun pop s = if isempty s then raise StackVacio
						else
							let val r = List.hd (!s)
							val _ = s := List.tl (!s)
						in r end
fun top s = if isempty s then raise StackVacio
						else (List.hd(!s))

fun stackToList s = !s

fun stackPP f s = "[" ^ (List.foldl (fn (x,pl) => (f x) ^ ";" ^ pl) "" (!s)) ^ "]"

end

