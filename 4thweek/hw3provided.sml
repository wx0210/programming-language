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

fun only_capitals xs = 
    List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

fun longest_string1 xs =
    foldl(fn (x,y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs =
    foldl(fn (x,y) => if String.size x < String.size y then y else x) "" xs

fun longest_string_helper f xs =
    foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" xs
 
val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x < y)

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

val count_wildcards = g (fn _ => 1) (fn x => 0)
			
val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x) 
	
fun count_some_var (str,pat) = g (fn _ => 0) (fn x => if x = str then 1 else 0) pat
    
fun check_pat (p : pattern) =
    let
	fun p2s (p : pattern) =
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
	has_repeats(p2s p)
    end

fun match (v : valu, p : pattern) =
    

					 
 

