(*      
    ;;    bigint.ml
	;;
    ;;    Riise Kennard
	;;
    ;;    CS112
	;;
*)


open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    (*Recursive Helpers*)

    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []                 ->  0
        | list1, []              ->  1
        | [], list2              -> -1
        | car1::cdr1, car2::cdr2 -> 
            let check = cmp' cdr1 cdr2
            in if check = 0 && car1 != car2
               then (if car1 > car2
                    then 1
                    else -1)
              else check

    let rec trimzeros list = match list with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = trimzeros cdr
                 in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
        	let sum = car1 + car2 + carry
        	in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec into base value count =
    	if (cmp' value base = 1)
    	then base, 0
   		else let check = add' value value 0
    		in if (cmp' check base = 1)
    			then value, count
    			else into base check (count+1)

    let rec doubler value count = match (value, count) with
    	| value, 0      -> value
    	| value, count  -> (doubler (add' value value 0) (count-1))

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
    	| [], _, _           -> []
        | list1, [], 0       -> list1
        | list1, [], carry   -> sub' list1 [carry] 0
        | car1::cdr1, car2::cdr2, carry ->
          let dif = car1 - carry - car2
          in (if dif < 0
          	then dif + 10 :: sub' cdr1 cdr2 1
          	else dif :: sub' cdr1 cdr2 0)

    let rec mul' value base sum = match (value, base, sum) with
        | [], _, sum         -> sum
        | [1], base, sum  -> add' base sum 0
        | value, base, sum -> 
        	let num, count = into value [2] 1
    		in mul' (trimzeros (sub' value num 0)) base (add' sum (doubler base count) 0)

    let rec squarer value count = match (value, count) with
    	| value, 0      -> value
    	| value, count  -> (squarer (mul' value value []) (count-1))

    let rec divrem' dividend divisor sum =
        if (cmp' dividend [] = 0)
        then sum, [0]
    	else let num, count = into dividend divisor 1
    		in if count = 0
    			then sum, dividend
    			else divrem' (trimzeros (sub' dividend num 0)) divisor (add' sum (doubler [1] (count - 1)) 0)

    let rec pow' expo base prod = match (expo, base, prod) with
        | [], _, prod         -> Pos, prod
        | [1], base, prod  -> Neg, mul' base prod []
        | expo, base, prod -> 
        	let num, count = into expo [2] 1
    		in pow' (trimzeros (sub' expo num 0)) base (mul' prod (squarer base count) [])

    let cmp (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then (cmp' value1 value2)
        else if neg1 = Pos
            then 1
            else -1

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else (if (cmp' value1 value2 = 1)
        	then Bigint (neg1, sub' value1 value2 0)
        	else (if neg1 = Pos
        		then Bigint (Neg, sub' value1 value2 0)
        		else Bigint (Pos, sub' value1 value2 0)))

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
    	if neg1 = neg2
        then (if (cmp' value1 value2 = 1)
        	then Bigint (neg1, trimzeros (sub' value1 value2 0))
        	else (if neg1 = Pos
        		then Bigint (Neg, trimzeros (sub' value1 value2 0))
        		else Bigint (Pos, trimzeros (sub' value1 value2 0))))
        else Bigint (neg1, add' value1 value2 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let check = (cmp' value1 value2 = 1)
    	in (if neg1 = neg2
        	then (if check
        		then Bigint (Pos, mul' value2 value1 [])
        		else Bigint (Pos, mul' value1 value2 []))
        	else (if check
        		then Bigint (Neg, mul' value2 value1 [])
        		else Bigint (Neg, mul' value1 value2 [])))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
    	let sign, value = pow' value2 value1 [1]
    	in if neg1 = Pos
        	then Bigint (Pos, value)
        	else Bigint (sign, value)

    let divrem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, remainder = divrem' value1 value2 []
        in match (neg1, neg2) with
        	| Pos, Pos -> Bigint (Pos, quotient), Bigint (Pos, remainder)
        	| Neg, Pos -> Bigint (Neg, add' quotient [1] 0), Bigint (Pos, trimzeros (sub' value2 remainder 0))
        	| Pos, Neg -> Bigint (Neg, quotient), Bigint (Pos, remainder)
        	| Neg, Neg -> Bigint (Pos, add' quotient [1] 0), Bigint (Pos, trimzeros (sub' value2 remainder 0)) 

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let _, remainder = divrem (Bigint (neg1, value1)) (Bigint (neg2, value2))
        in remainder

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let quotient, _ = divrem (Bigint (neg1, value1)) (Bigint (neg2, value2))
        in quotient

end
