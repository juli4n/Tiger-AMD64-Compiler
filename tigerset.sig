signature tigerset =
sig

type 'a tSet

val newSet : ('a * 'a -> order) -> 'a tSet
val singletonSet : ('a * 'a -> order) -> 'a ->  'a tSet
val addElemSet : 'a tSet * 'a -> unit
val addListSet : 'a tSet * 'a list -> unit
val memberSet : 'a tSet * 'a -> bool
val peekSet : 'a tSet * 'a -> 'a option
val equalSet : 'a tSet * 'a tSet -> bool
val unionSet : 'a tSet * 'a tSet -> 'a tSet
val unionInSet : 'a tSet * 'a tSet -> unit
val intersectionSet : 'a tSet * 'a tSet -> 'a tSet
val differenceSet : 'a tSet * 'a tSet -> 'a tSet
val differenceInSet : 'a tSet * 'a tSet -> unit
val forallSet : ('a -> unit) -> 'a tSet -> unit
val elemFromSet : 'a tSet -> 'a
val listItemSet : 'a tSet -> 'a list
val isEmptySet : 'a tSet -> bool
val setEmptySet : 'a tSet -> unit
val cardinalSet : 'a tSet -> int

val setPP : ('a -> string) -> 'a tSet -> string

end
