use "hw1.sml";

val test1 = if is_older ((01,01,2001), (02,01,2001))
	    then "Test 1 passed"
	    else "Test 1 failed"

val test2 = if number_in_month([(3,4,2018), (9,4,2011), (3,2,2018)], 4) = 2
	    then "Test 2 passed"
	    else "Test 2 failed"
		     
val test3 = if number_in_months ([(3,4,2018), (9,4,2011), (3,2,2018)], [2,4]) = 3
	    then "Test 3 passed"
	    else "Test 3 failed"

val test4 = if dates_in_month ([(2,0,2013),(2,3,2013),(5,1,2014), (4,1,2012)],0) = [(2,0,2013)]
	    then "Test 4 passed"
	    else "Test 4 failed"
		     
val test5 = if dates_in_months([(2,0,2013),(2,3,2013),(5,1,2014), (4,1,2012)],[0,3]) = [(2,0,2013),(2,3,2013)]
	    then "Test 5 passed"
	    else "Test 5 failed"
		     
val test6 = if get_nth (["Hello", "Hi"], 2) = "Hi"
	    then "Test 6 passed"
	    else "Test 6 failed"

val test7 = if date_to_string(9, 9, 2009) = "September-9-2009"
	    then "Test 7 passed"
	    else "Test 7 failed"

val test8 = if number_before_reaching_sum (7, [1,2,3,4,5]) = 3
	    then "Test 8 passed"
	    else "Test 8 failed"

val test9 = if what_month (53) = 2
	    then "Test 9 passed"
	    else "Test 9 failed"

val test10 = if month_range (58, 61) = [2,2,3,3]
	     then "Test 10 passed"
	     else "Test 10 failed"
		      
val test11 = if oldest ([(2,10,2019), (2,8,2019), (1,3,2018)]) = SOME (1,3,2018)
	     then "Test 11 passed"
	     else "Test 11 failed"

val test12 = if cumulative_sum ([12,27,13]) = [12,39,52]
	     then "Test 12 passed"
	     else "Test 12 failed"

