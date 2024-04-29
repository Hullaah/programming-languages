fun alternate(numbers: int list) =
let
  fun calculate_alternate(numbers: int list, i: int) =
    if null numbers
    then 0
    else
      if i mod 2 = 0
      then hd numbers + calculate_alternate(tl numbers, i + 1)
      else ~(hd numbers) + calculate_alternate(tl numbers, i + 1)
in
  calculate_alternate(numbers, 0)
end

fun min_max(numbers: int list) =
  if null (tl numbers)
  then (hd numbers, hd numbers)
  else
    let
      val min_max_pair = min_max(tl numbers)
    in
      if hd numbers < (#1 min_max_pair)
      then (hd numbers, (#2 min_max_pair))
      else if hd numbers > (#2 min_max_pair)
      then((#1 min_max_pair), hd numbers)
      else min_max_pair
    end

fun cum_sum(numbers: int list) =
  if null numbers orelse null (tl numbers)
  then numbers
  else hd numbers :: cum_sum(hd numbers + hd (tl numbers) :: tl (tl numbers))

fun greeting(name: string option) =
  if isSome name
  then "Hello there, " ^ valOf name ^ "!"
  else "Hello there, you!"

fun repeat(numbers: int list, amount_list: int list) =
let
  fun base_repeat(number: int, amount: int) =
    if amount = 0
    then []
    else number :: base_repeat(number, amount - 1)
  fun append(list1: int list, list2: int list) =
    if null list1 then list2 else hd list1 :: append(tl list1, list2)
in
  if null numbers
  then []
  else append(base_repeat(hd numbers, hd amount_list), repeat(tl numbers, tl
  amount_list))
end

fun addOpt(num1: int option, num2: int option) =
  if isSome num1 andalso isSome num2
  then SOME ((valOf num1) + (valOf num2))
  else NONE

fun addAllOpt(numbers: int option list) =
let
  fun check(numbers: int option list) =
    if null numbers
    then false
    else (isSome (hd numbers) orelse check (tl numbers))
in
  if check numbers
  then NONE
  else
    let val x = valOf (hd numbers)
    in
      if not (isSome x)
      then 0
      else valOf x + addAllOpt(tl numbers)
    end
end

