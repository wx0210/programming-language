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
    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
