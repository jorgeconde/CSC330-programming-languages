(*  Assignment #1 
	CSC 330

	Jorge Conde
	V00723209

	Jan 15, 2015
*)

type DATE = {year:int, month:int, day: int}
exception InvalidParameter

(* This file is where your solutions go *)

fun is_older(d1: DATE, d2: DATE): bool =
	if #year d1 > #year d2 
	orelse (#year d1 = #year d2 andalso #month d1 > #month d2) 
	orelse (#year d1 = #year d2 andalso #month d1 = #month d2 andalso #day d1 > #day d2)
	then true
	else false

fun number_in_month(dates: DATE list, month: int) =
	if null dates
	then 0
	else if #month (hd dates) = month
	then 1 + number_in_month(tl dates, month)
	else 0 + number_in_month(tl dates, month)

fun number_in_months(dates: DATE list, months: int list) =
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: DATE list, month: int) =
	if null dates
	then []
	else if #month (hd dates) = month
	then hd dates :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)


fun dates_in_months(dates: DATE list, months: int list) =
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) =
	let fun nth(index: int, strlist: string list) =
		if null strlist
		then raise InvalidParameter
		else if index = n
		then hd strlist
		else nth(index + 1, tl strlist)
	in
		if n = 0
		then raise InvalidParameter
		else nth (1, strings)
	end

fun date_to_string(date: DATE) = 
	let val month_names = ["January","February","March","April","May","June","July","August","September","October","November","December"]
	in
		get_nth(month_names, #month date) ^ " " ^ Int.toString(#day date) ^ ", " ^ Int.toString(#year date)
	end


fun number_before_reaching_sum(sum: int, l: int list) = 
	if null l
	then 0
	else if hd l >= sum
	then 0
	else
		let fun add(a1: int, l2: int list, sum2: int) =
			if sum2 <= 0
			then a1
			else add(a1 + 1, tl l2, sum2 - hd l2)
		in
			add(0, tl l, sum - hd l)
		end


fun what_month(day: int) =
	let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		if day < 1
		then day
		else number_before_reaching_sum(day, days_in_month) + 1
	end

fun month_range(d1: int, d2: int) =
	if d1 > d2
	then []
	else what_month(d1) :: month_range(d1+1, d2)

fun oldest(dates: DATE list) =
	if null dates
	then NONE
	else
		let val tl_oldest = oldest(tl dates)
		in
			if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates)
			then tl_oldest
			else SOME (hd dates)
		end

fun reasonable_date(date: DATE) =
	if #year date < 0 orelse #month date <= 0 orelse #month date > 12 
	then false
	else 
		let fun leap_year(year: int) =
			((year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0))
		in
			let val days_in_month = [31,if leap_year(#year date) then 29 else 28,31,30,31,30,31,31,30,31,30,31,31,31,31]

			in
				if List.nth(days_in_month, #month date - 1) >= #day date andalso #day date > 0
				then true
				else false
			end
		end 

