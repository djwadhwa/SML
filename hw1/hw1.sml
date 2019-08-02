fun is_older (date1: int*int*int, date2 : int*int*int) =
    if (#3 date1 <= #3 date2)
    then if (#2 date1 <= #2 date2)
	 then if (#1 date1 < #1 date2)
	      then true
	      else false
	 else false
    else false
	     
fun number_in_month (dates : (int*int*int) list, month: int) =
    if (null dates)
    then 0
    else if (#2 (hd dates) = month)
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
    if (null months)
    then 0
    else number_in_month (dates, hd months) + number_in_months(dates, tl months)
							      
fun dates_in_month (dates: (int*int*int) list, month: int)=
    if (null dates)
    then []
    else if (#2 (hd dates) = month)
    then (hd dates)::dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if (null months)
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (string_list: string list, n: int) =
    if n = 1
    then hd string_list
    else get_nth(tl string_list, n-1)

fun date_to_string (month: int, date: int, year: int) =
    let val months = ["January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth (months , month)^"-"^Int.toString date^"-"^Int.toString year
    end
fun number_before_reaching_sum (sum: int, int_list: int list) =
    if  sum-hd int_list <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd int_list, tl int_list)

fun what_month (days: int) =
    number_before_reaching_sum (days, [31,28,31,30,31,30,31,31,30,31,30,31])+1

fun month_range (day1: int, day2: int) =
    if (day1 > day2)
    then []
    else what_month (day1)::month_range(day1+1, day2)

fun oldest (date_list: (int * int * int) list) =
	   if null (date_list)
	   then NONE
	   else let val find_oldest = oldest (tl date_list)
		in if is_older(if isSome find_oldest then valOf find_oldest else hd date_list, hd date_list)
		   then find_oldest
		   else SOME (hd date_list)
		end


fun cumulative_sum (num_list: int list) =
    if null num_list
    then []
    else let fun sum (num: int, num_list: int list) =
		 if null num_list
		 then []
		 else (num + hd num_list) :: tl num_list
	 in hd num_list :: cumulative_sum (sum (hd num_list, tl num_list))
end
