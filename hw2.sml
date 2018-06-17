(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1 solutions *)

(* a. *)
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


(* 

b.
Return a list of names that can be substitions. 

*)
fun get_substitutions1(matrix: string list list, w: string) =
	case matrix of
		[] => []
	| 	xs::ys => case all_except_option(w, xs) of
					  SOME l => l @ get_substitutions1(ys, w)
					| NONE => get_substitutions1(ys, w)


(* 

c. 
Same as get_substitutions1, but using tail recursion.

*)
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

(* d. *)
type fullName = {first: string, last: string, middle: string}

fun similar_names(names: string list list, name: fullName) =
	let fun make_full_names(subNames: string list) = 
		case subNames of
			[] => []
			| h::t => case name of
				{first = x, last = y, middle = z} => ({first = h, last = y, middle = z} :: make_full_names(t))
	in
		case name of
			{first = x, last = y, middle = z} => {first = x, last = y, middle = z} :: make_full_names(get_substitutions1(names, x))
	end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Pproblem 2 solutions *)

(* 

a.
Takes a card and returns its color 

*)
fun card_color(card: card) = 
	case card of 
		  (Diamonds, _) => Red
		| (Hearts, _) => Red
		| (Clubs, _) => Black
		| (Spades, _) => Black


(*

b.
Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. 

*)

fun card_value(card: card) = 
	case card of
		  (_, Jack) => 10
		| (_, Queen) => 10
		| (_, King) => 10
		| (_, Ace) => 11
		| (_, Num (x)) => x


(*

c.
Takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c 

Remove c only once.
Throws ex if c is not in the list.

*)

fun remove_card(cards: card list, card: card, ex) = 
	case cards of
		  [] => raise ex
		| h::[] => if(h = card) then [] else h::[]
		| h::t => if(h = card) then t else h :: remove_card(t, card, ex)  


(*

d.
Takes a list of cards and returns true if all the cards in the
list are the same color

What if the list is empty?

*)
fun all_same_color(cards) =
	case cards of
		(* todo: fix nonexaustive match*)
		  h::[] => true					(* One element  *)
		| c1::c2::[] => card_color(c1) = card_color(c2) (* Two Elements *)   		
		| c1::c2::t => (card_color(c1) = card_color(c2)) andalso all_same_color(t) (* Three elements or more*)		
		| [] => true

(*

e. 
Takes a list of cards and returns the sum of their values
Use a recursive tail helper function

*)
fun sum_cards(cards) =
	let fun acc(cs, sum) = 
		case cs of
			 [] => sum
			| c1::t => acc(t, sum + card_value(c1)) (* todo: review tail recursion *)
	in
		acc(cards, 0)
	end 

(*

f.
Takes a card list (the held-cards) and an int (the goal) and computes
the score.

*)
fun score(cards, goal) =
	let val sum = sum_cards(cards)
		val preliminaryScore = if(sum > goal) then (3 * (sum - goal)) else (goal - sum)
	in
		case all_same_color(cards) of
			  true => preliminaryScore div 2
			| false => preliminaryScore
	end

(* g. *)

fun officiate(cards, moves, goal) =
	let fun state(held, mvs, cds: card list) =
		case (held, mvs, cds) of
			  (* Empty moves -> end of the game *)
			  (h::t, [], _) => score(h::t, goal)
			| ([], [], _) => score([], goal)

			(* Discard c *)
			| (h::t, Discard c::tail, a::b) => state(remove_card(h::t, c, IllegalMove), tail, a::b)
			| (h::t, Discard c::tail, []) => state(remove_card(h::t, c, IllegalMove), tail, [])

			(* Discard c with no held cards *)
			| ([], Discard c::tail, a::b) => state(remove_card([], c, IllegalMove), tail, a::b)
			| ([], Discard c::tail, []) => state(remove_card([], c, IllegalMove), tail, [])
			
			(* Draw in a empty card-list -> Game Over*)
			| (h::t, Draw::c, []) => score(h::t, goal)
			| ([], Draw::c, []) => score([], goal)


			(* Draw in a non-empty card-list -> Continue *)
			| (h::t, Draw::c, a::b) => state(a::h::t, c, b)
			| ([], Draw::c, a::b) => state(a::[], c, b)

		in
			state([], moves, cards)
		end
