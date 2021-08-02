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
	val subs = get_substitutions2(xss, a)
	fun local_helper [] = []
	  | local_helper (x :: xs) = [{first = x, middle = b, last = c}] @ local_helper(xs)
    in
	[{first = a, middle = b, last = c}] @ local_helper(subs)
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
      | false => x :: remove_card (xs, c, e)

fun all_same_color xs =
    case xs of
	[] => true
      | x :: [] => true
      | x :: xs =>
	let
	    val color = card_color(x)
	    fun check_same_color [] = true
	      | check_same_color (h :: hs) =
		case card_color (h) = color of
		    false => false
		  | true => check_same_color hs
	in
	    check_same_color xs
	end

fun sum_cards xs =
    let
	fun tail_helper ([], ans) = ans
	  | tail_helper (x :: xs', ans) = tail_helper(xs', card_value(x) + ans) 
    in
	tail_helper (xs,0)
    end

fun score(cards, goal) =
  let
    fun preliminary_score(sum) =
      case sum > goal
        of true => 3 * (sum - goal)
         | false => goal - sum

    fun final_score(pscore, true) = pscore div 2
      | final_score(pscore, false) = pscore
  in
    final_score(preliminary_score(sum_cards(cards)), all_same_color(cards))
  end

fun officiate(cards, moves, goal) =
  let
    fun replay_game(_, [], heldcards) = score(heldcards, goal)
      | replay_game([], Draw::moves, heldcards) = score(heldcards, goal)
      | replay_game(cards, Discard card::moves, heldcards) =
        replay_game(cards, moves, remove_card(heldcards, card, IllegalMove))
      | replay_game(card::restcards, Draw::moves, heldcards) =
        case sum_cards(card::heldcards) > goal
          of true => score(card::heldcards, goal)
           | false => replay_game(restcards, moves, card::heldcards)
  in
    replay_game(cards, moves, [])
  end
	
	
