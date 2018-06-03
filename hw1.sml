(* 1. *)
fun is_older(date1: int*int*int, date2: int*int*int) =
	if(#1 date1 <> #1 date2) 	   (* Year *)
	then (#1 date1 < #1 date2)
	else if(#2 date1 <> #2 date2)  (* Month *)
	then (#2 date1 < #2 date2) 
	else (#3 date1 < #3 date2)	   (* Day *)

(* 2. Return how many dates in the list are in the given month *)
fun number_in_month(dates: (int*int*int) list, month: int) =
	if (null dates) 
	then 0
	else
		if(#2 (hd dates) = month)
		then
			1 + number_in_month(tl dates, month)
		else
			number_in_month(tl dates, month)

(* 3.  *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
	if (null (tl months))
	then number_in_month(dates, hd months)
	else
		number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* 4. Return the list of dates in a given month *)
fun dates_in_month(dates: (int*int*int) list, month: int) = 
	if(null dates)
	then dates
	else if(#2 (hd dates) = month)
		 then hd dates :: dates_in_month(tl dates, month)
		 else dates_in_month(tl dates, month)

(* 5. Return all the dates whose month is in months *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
	(* Didn't know I could use @ operator. *)
	(*let fun concat(list1: (int*int*int) list, list2: (int*int*int) list) =
		if(null list1)
		then list2
		else hd list1 :: concat(tl list1, list2)
	in*)
	if(null months)
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
	

(* 6. Return the nth element of a list  *)
fun get_nth(list1: string list, n: int) =
	if(n = 1)
	then hd list1
	else get_nth(tl list1, n - 1)

(* 7. Convert date in a human redable format. *)
fun date_to_string(date: int*int*int) =
	get_nth(["January", "February", "March", "April", "May", 
			 "June", "July", "August", "September", "October", "November", "December"], #2 date) ^ 
	" " ^
	Int.toString(#3 date) ^
	", " ^
	Int.toString(#1 date)

(* 8. *)
(* todo: I think this is wrong. fix. *)
(* todo: do I have to consider the 0 case? *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
	if((sum) <= 0)
	then 0
	else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

(* 9. Return the month of the day. *)
(* What if I call what_month(31) ?? *)
fun what_month(day: int) =
	let
		val numbers = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum(day, numbers)
	end

(* 10. *)
fun month_range(day1: int, day2: int) =
	let fun range(list1: int list, countDown: int) =
		if(countDown < day1)
		then list1
		else range(what_month(countDown) :: list1, countDown - 1)
	in
		range([], day2)
	end

(* 11. *)
fun oldest(list1: (int*int*int) list) =
	let fun older(date: (int*int*int), remainingDates: (int*int*int) list) =
		if(null remainingDates)
		then date
		else
			if(is_older(hd remainingDates, date))
			then older(hd remainingDates, tl remainingDates)
			else older(date, tl remainingDates)
	in
		if(null list1)
		then NONE
		else SOME(older(hd list1, tl list1))
	end
