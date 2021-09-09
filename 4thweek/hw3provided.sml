(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x,0)))

val longest_string1 = foldl(fn (x,y) => if String.size x > String.size y then x else y) ""

val longest_string2 = foldl(fn (x,y) => if String.size x < String.size y then y else x) ""

fun longest_string_helper f = foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) ""
 
val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x :: xs' => case f x of
			SOME v => v
		      | NONE => first_answer f xs' 

fun all_answers f xs =
    let
	fun helper_function acc xs =
	    case xs of
		[] => SOME acc
	      | x :: xs' => case f x of
				SOME lst => helper_function (acc @ lst) xs'
			      | NONE => NONE				    
    in
	helper_function [] xs
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)
			
val count_wild_and_variable_lengths = g (fn () => 1) (String.size) 
	
fun count_some_var (str,pat) = g (fn () => 0) (fn x => if x = str then 1 else 0) pat
    
fun check_pat p =
    let
	fun p2s p =
	    case p of
	        Variable x => [x]
	      | TupleP ps => List.foldl (fn (x,acc) => acc @ (p2s x)) [] ps
	      | ConstructorP (_,p') => p2s p'
	      | _ => []

        fun has_repeats xs =
	    case xs of
		[] => false
	     |  x :: xs' => (List.exists (fn y => y = x) xs') orelse (has_repeats xs')
									 
    in
	not (has_repeats(p2s p))
    end

fun match (v,p) =
    case (v,p) of
	(_,Wildcard) => SOME []
      | (v,Variable s) => SOME [(s,v)]
      | (Unit,UnitP) => SOME []
      | (Const x,ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs,TupleP ps) => if List.length vs = List.length ps
				then all_answers (fn (vs',ps') => match (vs',ps')) (ListPair.zip(vs,ps))
				else NONE
      | (Constructor(s2,v),ConstructorP(s1,p)) => if s1 = s2 then match (v,p) else NONE
      | _ => NONE 
			       
fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
