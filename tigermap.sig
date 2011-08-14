signature tigermap =
sig

type ('key,'a) mapT


val newMap : ('key * 'key -> order) -> ('key,'a) mapT
val insertMap : ('key,'a) mapT * 'key * 'a -> unit
val peekMap : ('key,'a) mapT * 'key -> 'a option
val findMap : ('key,'a) mapT * 'key -> 'a
val updateMap : ('a -> 'a) -> 'key -> ('key, 'a) mapT -> unit
val listItemMap : ('key,'a) mapT -> ('key * 'a) list

val transformMap : ('a -> 'a) -> ('key,'a) mapT -> unit

val mapPP : ('key -> string) -> ('a -> string) -> ('key,'a) mapT -> unit

val setEmptyMap : ('key,'a) mapT ->  ('key * 'key -> order) -> unit

end
