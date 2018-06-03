(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1_1 = is_older ((2000,2,5),(2000,2,4)) = false
val test1_2 = is_older ((1,12,3),(0,3,4)) = false
val test1_3 = is_older ((3,2,4),(3,3,4)) = true


val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_1 = number_in_month ([(2012,2,28),(2013,12,1)],7) = 0
val test2_2 = number_in_month ([(2012,1,28)],1) = 1
val test2_3 = number_in_month ([(2012,2,28), (2012,2,28), (2012,2,28),(2013,12,1), (2012,2,28)], 2) = 4


val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[4]) = 1
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[7]) = 0
val test3_3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[1,2,3,4,5,6]) = 3
val test3_4 = number_in_months ([],[1,2,3,4,5,6]) = 0


val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_1 = dates_in_month ([],2) = []
val test4_2 = dates_in_month ([(2012,2,28)], 2) = [(2012,2,28)]
val test4_3 = dates_in_month ([(2012,2,28),(2013,12,1)], 3) = []


val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_1 = dates_in_months ([],[2,3,4]) = []
val test5_2 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test5_3 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[7, 5]) = []


val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_2 = get_nth (["hi"], 1) = "hi"
val test6_3 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"


val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test88 = number_before_reaching_sum(31, [31, 28, 30]) = 0
val test87 = number_before_reaching_sum(32, [31, 28, 30]) = 1
val test86 = number_before_reaching_sum(59, [31, 28, 30]) = 1
val test8 = number_before_reaching_sum (10, [1,2,3,4,5])  = 3

(*val test8_1 = number_before_reaching_sum (3, [1,2]) = 1*)
(*val test8_2 = number_before_reaching_sum (1, [0,2]) = 0*)

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([(2012,2,28),(2012,2,28),(2012,2,28)]) = SOME (2012,2,28)

