
(* 1. *)
fun is_older(date1: int*int*int, date2: int*int*int) =
	(#1 date1 < #1 date2) orelse (* Year *)
	(#2 date1 < #2 date2) orelse (* Month *)
	(#3 date1 < #3 date2)		 (* Day *)

val test1 = is_older ((1,2,3),(2,3,4)) = true


(* 2. Return how many dates in the list are in the given month *)
fun number_in_month(dates: (int*int*int) list, month: int) =
	if (null dates) 
	then 0
	else
		if(#2 (hd dates) = month)
		then
			1 + number_in_month(tl dates, month)
		else
			0 + number_in_month(tl dates, month)

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1


(* 3.  *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
	if (null (tl months))
	then number_in_month(dates, hd months)
	else
		number_in_month(dates, hd months) + number_in_months(dates, tl months)


val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3


(* 4. Return the list of dates in a given month *)
fun dates_in_month(dates: (int*int*int) list, month: int) = 
	if(null dates)
	then dates
	else if(#2 (hd dates) = month)
		 then hd dates :: dates_in_month(tl dates, month)
		 else dates_in_month(tl dates, month)

val test4 = dates_in_month ([(2012,2,28),(2013,12,1), (2014, 2, 1)],2) = [(2012,2,28), (2014, 2, 1)]


(* 5. Return all the dates whose month is in months *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =

	let fun concat(list1: (int*int*int) list, list2: (int*int*int) list) =
		if(null list1)
		then list2
		else hd list1 :: concat(tl list1, list2)
	in
		if(null months)
		then []
		else concat(dates_in_month(dates, hd months), dates_in_months(dates, tl months))
	end

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

