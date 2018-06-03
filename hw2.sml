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
			| xs::ys => if(same_string(xs, w)) then ys else xs :: extract(ys)
	in
		case (tokens, extract(tokens)) of
			  (a::[], []) => SOME [] 
			| (a::b, c::d) => if(a = c andalso b = d) then NONE else SOME(c::d)
			| _ => NONE	
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

(* 3. same as get_substitutions1, but using tail recursion *)
fun get_substitutions2(matrix: string list list, w: string) =
	let fun aux(rest: string list list, acc: string list) =
		case rest of
		  [] => acc
		| hd::tl => (case all_except_option(w, hd) of
			  SOME l => aux(tl, acc @ l)
			| NONE => aux(tl, acc ) )
	in
		aux(matrix, [])
	end


(*

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})

answer: 

[{first="Fred", last="Smith", middle="W"},
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}]

*)

(* 1. Get list for first name using get_substitutions2(names, firstName) *)
(* 2. Make a racursion at that list *)

type fullName = {first: string, last: string, middle: string}
let fun similar_names(names: string list list, name: fullName) =
	