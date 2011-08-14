structure tigerutils =
struct

fun id x = x

fun acumMap f g a ls = #2(List.foldl (fn (x,(n,b)) => (g n, (f (x,n))::b)) (a,[]) ls)

fun itoa x = (if x<0 then "-" else "")^(Int.toString (Int.abs x))

fun flip f (x,y) = f (y,x)

fun checkDistintos l = 
	let
		fun f [] = true
		  | f (x::[]) = true
      | f (x::y::xs) = if x <> y then f (y::xs) else false
		val lsort = Listsort.sort String.compare l
	in f lsort end

fun curry f x y = f(x,y)

fun uncurry f (x,y) = f x y

fun listn a 0 = []
  | listn a n = a::(listn a (n-1))


fun listTo n = let
		fun aux 0 = []
			| aux n = n :: (aux (n-1))
		in List.rev (aux n) end

fun init [] = []
	| init (x::[]) = []
	| init (x::y::ls) = x::(init (y::ls))

end
