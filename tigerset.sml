structure tigerset :> tigerset =
struct

open Splayset

type 'a tSet = 'a Splayset.set ref

fun newSet cmp = ref (Splayset.empty cmp)
fun singletonSet cmp a = ref (Splayset.singleton cmp a)
fun addElemSet (set,a) = set := Splayset.add (!set,a) 
fun addListSet (set,ls) = set := Splayset.addList (!set,ls)
fun memberSet (set,a) = Splayset.member (!set,a)
fun peekSet (set,a) = Splayset.peek (!set,a)
fun equalSet (s1,s2) = Splayset.equal (!s1,!s2)
fun unionSet (s1,s2) = ref (Splayset.union (!s1,!s2))
fun unionInSet (s1,s2) = s1 := Splayset.union (!s1,!s2)
fun intersectionSet (s1,s2) = ref (Splayset.intersection (!s1,!s2))
fun differenceSet (s1,s2) = ref (Splayset.difference (!s1,!s2))
fun differenceInSet (s1,s2) = s1 := Splayset.difference (!s1,!s2)
fun forallSet f s  = Splayset.app f (!s)
fun updateSet f set = set := f set
fun elemFromSet s = List.hd (Splayset.listItems (!s))

fun listItemSet s = Splayset.listItems (!s)

fun isEmptySet s = Splayset.isEmpty (!s)

fun setPP f s= 
	let
		val l = List.map (fn a => (f a) ^ ",") (Splayset.listItems (!s))
	in "{" ^ (String.concat l) ^ "}"  end

fun setEmptySet s = differenceInSet(s,s)

fun cardinalSet s = numItems (!s) 

end
