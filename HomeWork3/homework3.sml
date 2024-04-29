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
val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0)))

val foldl = List.foldl

val longest_string1 = foldl (fn (x, y) => if String.size x > String.size y then x else y) ""

val longest_string2 = foldl (fn (x, y) => if String.size x >= String.size y then x else y) ""

fun longest_string_helper f lst =
  foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" lst

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = (longest_string1 o only_capitals)

val rev_string = String.implode o rev o String.explode

fun first_answer f  lst =
  case lst of
       [] => raise NoAnswer
     | x :: lst' => case f x of
                         NONE => first_answer f lst'
                       | SOME v => v

fun all_answers f lst =
  case lst of
       [] => SOME []
     | x::lst' => case (f x, all_answers f lst') of
                       (SOME v, SOME u) => SOME (v @ u)
                     | _ => NONE

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (s, p) =
  g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
let
  fun make_list p =
    case p of
         Variable s => s::[]
       | TupleP ps => foldl (fn (x, y) => (make_list x) @ y) [] ps
       | ConstructorP (_, x) => make_list x
       | _ => []

  fun are_distinct l =
    case l of
         [] => false
       | x::[] => true
       | x::l' => not (List.exists (fn y => y = x) l') andalso are_distinct l'
in
  (are_distinct o make_list) p
end

fun match x =
  case x of
       (_, Wildcard) => SOME []
     | (Unit, UnitP) => SOME []
     | (Const y, ConstP z) => if y = z then SOME [] else NONE
     | (y, Variable z) => SOME [(z, y)]
     | (Tuple vs, TupleP ps) => if List.length ps = List.length vs
                                then all_answers match (ListPair.zip (vs, ps))
                                else NONE
     | (Constructor (x1, y1), ConstructorP (x2, y2)) => if x1 = x2
                                                        then match (y1, y2)
                                                        else NONE
     | _ => NONE

fun first_match v ps =
  SOME (first_answer (fn p => match (v, p)) ps) handle NoAnswer => NONE
