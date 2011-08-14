signature tigerintmap =
sig

type 'a intMap 

val newMap : unit -> 'a intMap
val insertMap : ('a intMap * int * 'a) -> unit
val findMap : ('a intMap * int ) -> 'a option
val eqMaps : ('a * 'a -> bool) -> ('a intMap * 'a intMap) -> bool
val listItems : 'a intMap -> (int * 'a) list


end
