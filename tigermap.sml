structure tigermap :> tigermap =
struct

type ('key,'a) mapT = ('key,'a) Splaymap.dict ref

fun newMap cmp = ref (Splaymap.mkDict cmp)

fun insertMap (m,k,a) = m := Splaymap.insert(!m,k,a)

fun peekMap (m,k) = Splaymap.peek(!m,k) 

fun updateMap f k m = case peekMap (m,k) of
									SOME v => insertMap (m,k,f v)
								| NONE => ()

fun findMap (m,k) = Splaymap.find(!m,k) 

fun listItemMap m = Splaymap.listItems (!m)

fun transformMap f m = m := (Splaymap.transform f (!m))

fun mapPP kf vf m = List.app (fn (k,v) => print ("["^(kf k)^"] => "^(vf v)^"\n")) (Splaymap.listItems (!m))

fun setEmptyMap m cmp = m := (Splaymap.mkDict cmp)

end
