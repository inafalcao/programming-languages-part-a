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
			0 + number_in_month(tl dates, month)

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

	let fun concat(list1: (int*int*int) list, list2: (int*int*int) list) =
		if(null list1)
		then list2
		else hd list1 :: concat(tl list1, list2)
	in
		if(null months)
		then []
		else concat(dates_in_month(dates, hd months), dates_in_months(dates, tl months))
	end

(* 6. Return the nth element of a list  *)
fun get_nth(list1: string list, n: int) =

	let fun accumulate(helperList: string list, accumulator: int) =
		if(accumulator = n)
		then hd helperList
		else accumulate(tl helperList, accumulator + 1)
	in
		accumulate(list1, 1)
	end

(* 7. Convert date in a human redable format. *)
fun date_to_string(date: int*int*int) =
	get_nth(["January", "February", "March", "April", "May", 
			 "June", "July", "August", "September", "October", "November", "December"], #2 date) ^ 
	" " ^
	Int.toString(#3 date) ^
	", " ^
	Int.toString(#1 date)

(* 8. *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
	let fun accumulate(nums: int list, acc: int, previous: int) =
		if(null (tl nums) orelse hd nums + acc >= sum)
		then previous
		else accumulate(tl nums, hd nums + acc, hd nums)
	in
		accumulate(tl numbers, hd numbers, hd numbers)
	end


(* 9. Return the month of the day. *)
fun what_month(day: int) =
	let
		val numbers = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

		fun accumulate(nums: int list, acc: int, month: int) =
			if(day <= acc) 
			then month 
			else
				if(null (tl nums) orelse hd nums + acc >= day)
				then month + 1
				else accumulate(tl nums, hd nums + acc, month + 1)

	in
		accumulate(tl numbers, hd numbers, 1)

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
