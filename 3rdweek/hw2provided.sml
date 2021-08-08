(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, xs) =
    case xs of
	[] => NONE
      | x :: xs' => if same_string (s, x)
		    then SOME xs'
		    else case all_except_option (s, xs') of
			     NONE => NONE
			   | SOME y => SOME(x :: y)
					  
fun get_substitutions1 ([], str) = []
  | get_substitutions1 (x :: xs, str) =
    case all_except_option(str, x)
      of NONE => get_substitutions1(xs, str)
      | SOME(y) => y @ get_substitutions1(xs, str) 
					 
fun get_substitutions2 ([] ,str) = []
  | get_substitutions2 (xss, str) =
    let
	fun tail_helper ([], ans) = ans
	  | tail_helper (x :: xs, ans) =
	    case all_except_option(str, x)
	     of NONE => tail_helper(xs, ans)
	     |  SOME(y) => tail_helper(xs, ans @ y)
    in
	tail_helper(xss, [])
    end
	
fun similar_names (xss, {first = a, middle = b, last = c}) =
    let
	fun local_helper [] = []
	  | local_helper (x :: xs) = {first = x, middle = b, last = c} :: local_helper(xs)
    in
	{first = a, middle = b, last = c} :: local_helper(get_substitutions2(xss, a))
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

fun card_color (card) =
    case card of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red

fun card_value (card) =
    case card of
	(_,Num x) => x
      | (_,Ace) => 11
      | (_,_) => 10 

fun remove_card ([], c, e) = raise e
  | remove_card (x :: xs, c, e) =
    case x = c of
	true => xs
      | false => x :: remove_card(xs, c, e)

fun all_same_color xs =
    case xs of
	[] => true
      | [_] => true
      | head :: neck :: tail => card_color head = card_color neck andalso all_same_color(neck :: tail) 

fun sum_cards xs =
    let
	fun tail_helper ([], ans) = ans
	  | tail_helper (x :: xs', ans) = tail_helper(xs', card_value(x) + ans) 
    in
	tail_helper (xs,0)
    end

fun score (xs, goal) =
    let
	val sum = sum_cards xs
    in
	(if sum >= goal then 3* (sum - goal) else goal - sum)
	    div (if all_same_color xs then 2 else 1)
    end

fun officiate (cards, plays, goal) =
    let
	fun loop (current_cards, cards_left, plays_left) =
	    case plays_left of
		[] => score(current_cards,goal)
	      | (Discard c) :: tail  => loop(remove_card(current_cards,c,IllegalMove),cards_left,tail)
	      | Draw :: tail => case cards_left of
				    [] => score(current_cards,goal)
				  | c::rest => if sum_cards(c :: current_cards) > goal
					       then score(c :: current_cards,goal)
					       else loop((c :: current_cards), rest, tail)	    
    in
	loop([],cards,plays)
    end
	
(* Chanllenge Problems*)

fun score_challenge (xs,goal) =
    let
	val raw_score = score(xs,goal)
	fun best_score (score_list, best) =
	    case score_list of
		[] => best
	      | s :: score_list' => if s < best
				    then best_score(score_list', s)
				    else best_score(score_list', best)

	fun get_score preliminary =
	    (if preliminary > goal then 3*(preliminary -goal) else goal - preliminary) div (if all_same_color(xs) then 2 else 1)

	fun possible_score (xs,acc,prev) =
	    case xs of
		[] => acc
	      | (_,Ace) :: xs' =>possible_score (xs', get_score(prev-10) :: acc, prev -10)
	      | _ :: xs' => possible_score(xs',acc,prev)	    
    in
	best_score(possible_score(xs,[raw_score],raw_score),raw_score)
    end
	


	
