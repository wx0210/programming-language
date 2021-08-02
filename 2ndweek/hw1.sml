fun is_older(first_date : int*int*int, second_date : int*int*int) =
    let 
        val y1 = #1 first_date
        val m1 = #2 first_date
        val d1 = #3 first_date
        val y2 = #1 second_date
        val m2 = #2 second_date
        val d2 = #3 second_date
    in
        y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end

fun number_in_month(datelists : (int*int*int) list , mon : int ) =
    if null datelists
    then 0
    else 
        if #2 (hd datelists) = mon
        then 1+number_in_month(tl datelists, mon)
        else number_in_month(tl datelists, mon)

fun number_in_months(datelists : (int*int*int) list , mons : int list) =
    if null mons
    then 0
    else number_in_month(datelists, hd mons) + number_in_months(datelists, tl mons)

fun dates_in_month(dates : (int*int*int) list, month : int) = 
    if null dates
    then []
    else 
        if #2 (hd dates) = month
        then hd dates::dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(words : string list, num : int) = 
    if num = 1
    then hd words
    else get_nth(tl words, num-1)

fun date_to_string(date : int*int*int) =
    let
        val months_string=["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val month = get_nth(months_string,#2 date)
        val day = Int.toString(#3 date)
        val year = Int.toString(#1 date)
    in
        month^" "^day^", "^year
    end

fun number_before_reaching_sum(sum : int, nums_list : int list) =
    if sum <= hd nums_list
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums_list, tl nums_list)


fun what_month(day : int) =
    let
        val month_day = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day,month_day) + 1
    end

fun month_range(day1 : int, day2 :int) =
    if day1 <= day2
    then what_month(day1) :: month_range(day1 + 1,day2)
    else []

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else 
        let 
            fun oldest_helper(l:(int*int*int) list, older:(int*int*int)) =
                if null l
                then older
                else 
                    if is_older(older, hd l)
                    then oldest_helper(tl l, older)
                    else oldest_helper(tl l, hd l)
        in
            SOME(oldest_helper(tl dates, hd dates))
        end

fun is_in(date : int, l : int list) =
    if null l
    then false
    else 
        if date = hd l
        then true
        else is_in(date, tl l)

fun rmvduplicates(months : int list) = 
    if null months
    then []
    else 
        let
            val ans = rmvduplicates(tl months)
        in
            if is_in(hd months, ans) 
            then ans
            else hd months :: ans
        end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
    number_in_months(dates, rmvduplicates(months))
fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, rmvduplicates(months))


fun reasonable_date(date : int*int*int) =
    let
        val year = #1 date
        val month = #2 date
        val day = #3 date
        val leap = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val feb_len = if leap then 29 else 28
        val month_length= [31,feb_len,31,30,31,30,31,31,30,31,30,31]
        fun get_nth(days : int list, num : int) = 
            if num = 1
            then hd days
            else get_nth(tl days, num-1)
    in
        year > 0 andalso month > 0 andalso month < 13 
        andalso day > 0 andalso day < get_nth(month_length, month)
    end

