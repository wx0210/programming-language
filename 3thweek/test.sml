(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, []) = NONE
  | all_except_option (str, x :: xs) =
    case (same_string(str,x), all_except_option(str, xs))
	 of (true, _) => SOME(xs)
	  | (false, NONE) => NONE
	  | (false, SOME(y)) => SOME(x :: y)

fun get_substitutions1 ([], str) = []
  | get_substitutions1 (x :: xs, str) =
    case all_except_option(str, x)
      of NONE => get_substitutions1(xs, str)
      | SOME(y) => y @ get_substitutions1(xs, str) 
					 
fun get_substitutions2 ([] ,str) = []
  | get_substitutions2 (xss, str) =
    let
	fun tail_helper ([], _) = []
	  | tail_helper (x :: xs, ans) =
	    case all_except_option(str, x)
	     of NONE => tail_helper(xs, ans)
	     |  SOME(y) => tail_helper(xs, ans @ y)
    in
	tail_helper(xss, [])
    end

fun similar_names (xss, {first,middle,last}) =
    let 
        val subs = get_substitutions2(xss, first)
        fun local_helper [] = []
        | local_helper (x :: xs) = {first = x, middle = middle,last = last} @ local_helper(xs)
    in
        {first,middle,last} @ local_helper subs
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             