fun is_older(date1: int * int * int, date2: int * int * int) =
let
  val y1 = #1 date1
  val y2 = #1 date2
  val m1 = #2 date1
  val m2 = #2 date2
  val d1 = #3 date1
  val d2 = #3 date2
in
  if y1 < y2
  then true
  else if y1 = y2 andalso m1 < m2
  then true
  else if y1 = y2 andalso m1 = m2 andalso d1 < d2
  then true
  else false
end

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else
      if (#2 (hd dates)) = month
      then 1 + number_in_month (tl dates, month)
      else number_in_month (tl dates, month)

fun number_in_months(dates: (int * int * int) list, month: int list) =
  if null month
  then 0
  else number_in_month (dates, hd month) + number_in_months (dates, tl month)

fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else
    if (#2 (hd dates)) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month (tl dates, month)

fun append(first_list: (int * int * int) list, second_list: (int * int * int)
  list) =
  if null first_list
  then second_list
  else hd first_list :: append(tl first_list, second_list)

fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else append(dates_in_month (dates, hd months), dates_in_months (dates, tl months))

fun get_nth(word: string list, n: int) =
  if n = 1
  then hd word
  else get_nth(tl word, n - 1)

fun date_to_string(date: (int * int * int)) =
let
  val months = ["January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"]
in
  get_nth (months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^
  ", "  ^ Int.toString (#1 date)
end

fun number_before_reaching_sum(sum: int, numbers: int list) =
  if sum <= 0
  then ~1
  else 1 + number_before_reaching_sum (sum - hd numbers, tl numbers)

fun what_month(day: int) =
let
  val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
in
  1 + number_before_reaching_sum (day, months)
end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month (day1) :: month_range (day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else if null (tl dates)
  then SOME (hd dates)
  else if is_older (hd dates, hd (tl dates)) then oldest (hd dates :: tl (tl
  dates)) else oldest (hd (tl dates) :: tl (tl dates))

fun remove_duplicates(months: int list, months_without_duplicate: int list) =
  let
    fun contains(months, month) =
      if null months
      then false
      else if hd months = month then true else contains(tl months, month)
  in
    if null months
    then months_without_duplicate
    else
      if contains (months_without_duplicate, hd months)
      then remove_duplicates (tl months, months_without_duplicate)
      else remove_duplicates(tl months, hd months :: months_without_duplicate)
  end

fun number_in_months_challenge(dates: (int * int * int) list, months: int list)
  =
  number_in_months (dates, remove_duplicates (months, []))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
  dates_in_months(dates, remove_duplicates (months, []))

fun reasonable_date(date: (int * int * int)) =
let
  val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  val is_leap_year = (((#1 date) mod 400) = 0 orelse ((#1 date) mod 4 = 0)) andalso
  ((#1 date) mod 100) <> 0
  fun get_nth(months: int list, n: int) =
    if n = 1
    then hd months
    else get_nth(tl months, n - 1)
in
  if (#1 date) > 0 andalso (#2 date) >= 1 andalso (#2 date) <= 12
  then
    if is_leap_year andalso (#2 date) = 2
    then if (#3 date) <= 29 then true else false
    else (#3 date) > 0 andalso (#3 date) <= get_nth (days_in_months, #2 date)
  else false
end
