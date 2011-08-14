structure tigerintmap :> tigerintmap =
struct

open Intmap

type 'a intMap = 'a Intmap.intmap ref

fun newMap () = ref (Intmap.empty())

fun insertMap (m,i,a) = m := Intmap.insert (!m,i,a)

fun findMap (m,i) = Intmap.peek(!m,i) 

fun eqMaps f (m1,m2) = 
		let fun f' ((cl,vl),(cr,vr))= f (vl,vr)
		in ListPair.all f' (Intmap.listItems (!m1) , Intmap.listItems(!m2))
		end

fun listItems m = Intmap.listItems (!m)

end 
