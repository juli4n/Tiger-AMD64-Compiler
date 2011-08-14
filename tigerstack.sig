signature tigerstack =
sig

type 'a stack

exception StackVacio

val newstack : unit -> 'a stack
val push : 'a stack -> 'a -> 'a stack
val pop : 'a stack -> 'a
val top : 'a stack -> 'a
val isempty : 'a stack -> bool
val stackToList : 'a stack -> 'a list

val stackPP : ('a -> string) -> 'a stack -> string

end 
