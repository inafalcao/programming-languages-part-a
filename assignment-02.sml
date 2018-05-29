(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


(* 1. *)
fun all_except_option(w: string, tokens: string list) =
	let fun extract(tokens: string list) =
		case tokens of
			[] => []
		| 	xs::ys => if(same_string(xs, w)) then ys else xs :: extract(ys)
	ins
		case extract(tokens) of
			[] => (case tokens of [] => NONE | _ => SOME [])
		|	x::y => if(x::y = tokens) then NONE else SOME(x::y)
	end

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_0 = all_except_option ("a", ["a", "b", "c"]) = SOME ["b", "c"]
val test1_1 = all_except_option ("c", ["a", "b", "c"]) = SOME ["a", "b"]


(* 2 *)
fun get_substitutions1(matrix: string list list, w: string) =
	case matrix of
		[] => []
	| 	xs::ys => let val current = all_except_option(w, xs)
			in case current of
				SOME l => l @ get_substitutions1(ys, w)
			| 	NONE => get_substitutions1(ys, w)
			end

val test1 = all_except_option ("notstring", ["string"]) = NONE
val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
