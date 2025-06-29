type date = {
  day   : int;
  month : int;
  year  : int
}
type day_name = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                 
let divide p q =
  assert (p>0 && q>0);
  p mod q = 0

let leap_year = function
  | year when (divide year 4) && not (divide year 100) -> true
  | year when (divide year 400) -> true
  | _ -> false 

let nb_days_month date =
  match date.month with
  | 2 -> if leap_year date.year then 29 else 28
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | _ -> 30

let nb_days_end_month date =
  nb_days_month date - date.day

let next_month month =
  assert ((month >0) && (month <13));
  (month mod 12) + 1
  
let nb_days_end_year date =
  match date.month with
  | 12 -> nb_days_end_month date
  | 11 -> (nb_days_end_month date)+31
  | 10 -> (nb_days_end_month date)+30+31
  | 9  -> (nb_days_end_month date)+31+30+31
  | 8  -> (nb_days_end_month date)+30+31+30+31
  | 7  -> (nb_days_end_month date)+31+30+31+30+31
  | 6  -> (nb_days_end_month date)+31+31+30+31+30+31
  | 5  -> (nb_days_end_month date)+30+31+31+30+31+30+31
  | 4  -> (nb_days_end_month date)+31+30+31+31+30+31+30+31
  | 3  -> (nb_days_end_month date)+30+31+30+31+31+30+31+30+31
  | 2  -> (nb_days_end_month date)+31+30+31+30+31+31+30+31+30+31
  | 1  -> (nb_days_end_month date)+(if leap_year date.year then 29 else 28)+31+30+31+30+31+31+30+31+30+31
  | _ -> raise (Failure "Unrecognized month.")

let rec aux_add_days date days =
  assert (days <= nb_days_end_year date);
  let nb_days_end_month = nb_days_end_month date in
  if days > nb_days_end_month then
    aux_add_days {date with day=1; month=next_month date.month} (days-nb_days_end_month-1)
  else
    {date with day=date.day+days}


let rec add_days_to_date date days =
  let nb_days_end_year = nb_days_end_year date in 
  if days > nb_days_end_year then
    add_days_to_date {day=1; month=1; year=date.year+1} (days-nb_days_end_year-1) 
  else
    aux_add_days date days 

let string_of_month = function
  | 1  -> "January"
  | 2  -> "February"
  | 3  -> "March"
  | 4  -> "April"
  | 5  -> "May"
  | 6  -> "June"
  | 7  -> "July"
  | 8  -> "August"
  | 9  -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _  -> raise (Failure "Unrecognized month.")

let string_of_date date =
  string_of_month date.month^" "^string_of_int date.day^", "^string_of_int date.year

let is_before date1 date2 =
  if date1.year < date2.year then
    true
  else if date1.year > date2. year then
    false
  else if date1.month < date2.month then
    true
  else if date1.month > date2.month then
    false
  else if date1.day < date2.day then
    true
  else
    false
    
let nb_days_year year =
  if leap_year year then 366 else 365

let rec nb_days_between_two_years year1 year2 =
  if year1>year2 then
    0
  else if year1=year2 then
    nb_days_year year1
  else
    nb_days_year year1 + (nb_days_between_two_years (year1+1) year2)

let nb_days_beginnig_year date =
  match date.month with
  | 12 -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31+30+31+31+30+31+30
  | 11 -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31+30+31+31+30+31
  | 10 -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31+30+31+31+30
  | 9  -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31+30+31+31
  | 8  -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31+30+31
  | 7  -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31+30
  | 6  -> date.day+31+(if leap_year date.year then 29 else 28)+31+30+31
  | 5  -> date.day+31+(if leap_year date.year then 29 else 28)+31+30
  | 4  -> date.day+31+(if leap_year date.year then 29 else 28)+31
  | 3  -> date.day+31+(if leap_year date.year then 29 else 28)
  | 2  -> date.day+31
  | 1  -> date.day
  | _ -> raise (Failure "Unrecognized month.")
           
let diff_between_dates date1 date2 =
  if date1.year = date2.year then
    if is_before date1 date2 then
      nb_days_end_year date1 - nb_days_end_year date2
    else
      nb_days_end_year date2 - nb_days_end_year date1
  else if is_before date1 date2 then
    nb_days_end_year date1 + nb_days_between_two_years (date1.year+1) (date2.year-1) + nb_days_beginnig_year date2
  else
    nb_days_end_year date2 + nb_days_between_two_years (date2.year+1) (date1.year-1) + nb_days_beginnig_year date1

let string_of_day  = function
  | Monday -> "Monday"
  | Tuesday -> "Tuesday"
  | Wednesday -> "Wednesday"
  | Thursday -> "Thursday"
  | Friday -> "Friday"
  | Saturday -> "Saturday"
  | Sunday -> "Sunday"

let day_of_week date =
  let date_ref = {day=1; month=1; year=2000} in (* saturday *)
  let nb_diff = diff_between_dates date date_ref in
  let day =
    if is_before date_ref date then
      (nb_diff) mod 7
    else
      (7 - (nb_diff mod 7)) mod 7
  in
  match day with
  | 0 -> Saturday
  | 1 -> Sunday
  | 2 -> Monday
  | 3 -> Tuesday
  | 4 -> Wednesday
  | 5 -> Thursday
  | 6 -> Friday
  | _ -> raise (Failure "Unrecognized day.")
   
let () =
  if (Array.length Sys.argv) = 5 then
    let date = {day = int_of_string Sys.argv.(2); month = int_of_string Sys.argv.(1); year = int_of_string Sys.argv.(3)} in
    let days = int_of_string Sys.argv.(4) in
    let new_date = add_days_to_date date days in
    Printf.printf "%s + %d days = %s\n" (string_of_date date) days (string_of_date new_date)
  else if (Array.length Sys.argv) = 4 then
    let date = {day = int_of_string Sys.argv.(2); month = int_of_string Sys.argv.(1); year = int_of_string Sys.argv.(3)} in
    Printf.printf "%s is a %s.\n" (string_of_date date) (string_of_day (day_of_week date))
  else
    let stallman = {day=16;month=3;year=1953} in
    let torvalds = {day=28;month=12;year=1969} in
    let turing1 = {day=23;month=6; year=1912} in
    let turing2 = {day=7;month=6; year=1954} in
    let lovelace = {day=10;month=12; year=1815} in
    Printf.printf "Stallman - Torvalds : %d days\n" (diff_between_dates stallman torvalds);
    Printf.printf "Turing : %d days\n" (diff_between_dates turing1 turing2);
    Printf.printf "Lovelace : %s is a %s.\n" (string_of_date lovelace) (string_of_day (day_of_week lovelace))

