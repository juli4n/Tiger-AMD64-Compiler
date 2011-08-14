structure tigertree = 
struct

open tigertemp
open tigerutils

datatype exp = 	CONST of int
							| NAME of tigertemp.label
							| TEMP of tigertemp.temp
							| BINOP of binop * exp * exp
							| MEM of exp
							| CALL of exp * exp list
							| ESEQ of stm * exp

and stm = MOVE of exp * exp
				| EXP of exp
				| JUMP of exp * tigertemp.label list
				| CJUMP of relop * exp * exp * tigertemp.label * tigertemp.label
				| SEQ of stm * stm
				| LABEL of tigertemp.label

and binop = PLUS | MINUS | MUL | DIV
						| AND | OR | LSHIFT | RSHIFT 
						| ARSHIFT | XOR

and relop = EQ | NE | LT | GT | LE | GE
						| ULT | ULE | UGT | UGE


fun seq l = List.foldr (fn (x,y) => SEQ(x,y)) (List.last l) (init l)

fun notRel r = case r of
				EQ => NE
			| NE => EQ
			| LT => GE
			| GT => LE
			| LE => GT
			| GE => LT
			| ULT => UGE
			| ULE => UGT
			| UGT => ULE	
			| UGE => ULT 

end
