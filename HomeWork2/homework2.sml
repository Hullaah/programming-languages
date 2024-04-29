(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, ss) =
let
  fun check (ss) =
    case ss of
         [] => false
       | s1 :: ss' =>  same_string(s1, s) orelse check (ss')

  fun helper (ss) =
    case ss of
         [] => NONE
       | s1 :: [] => if same_string(s1, s) then SOME [] else SOME (s1 :: [])
       | s1 :: ss' => case helper(ss') of
                                   NONE => NONE
                                 | SOME z => if same_string(s1, s) then SOME z
                                             else SOME (s1 :: z)
in
  if check (ss) then
    helper (ss)
  else NONE
end

fun get_substitutions1 (substitutions, name) =
  case substitutions of
       [] => []
     | substitution :: substitutions' => (case all_except_option(name, substitution) of
                                              NONE => []
                                            | SOME substitute => substitute)
                                           @ get_substitutions1(substitutions',
                                           name)

fun get_substitutions2 (substitutions, name) =
let
  fun aux (substitutions, acc) =
    case substitutions of
         [] => acc
       | substitution :: substitutions' => aux (substitutions', acc @ (case all_except_option(name,
       substitution) of
                                                 NONE => []
                                               | SOME substitute => substitute))
in
  aux (substitutions, [])
end

fun similar_names (substitutions, {first=firstname, middle=middlename, last=lastname}) =
  let
    fun get_similar_names (substitutes) =
      case substitutes of
           [] => []
         | substitute :: substitutes' => [{first=substitute, middle=middlename,
         last=lastname}] @ get_similar_names (substitutes')
  in
    {first=firstname, middle=middlename, last=lastname} :: get_similar_names (
    get_substitutions2 (substitutions, firstname))
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
fun card_color (c_suit, c_rank) =
  case c_suit of
       Spades => Black
     | Clubs => Black
     | _ => Red

fun card_value (c_suit, c_rank) =
  case c_rank of
       Num n => n
     | Ace => 11
     | _ => 10

fun remove_card (cs, c, e) =
    case cs of
         [] => raise e
       | c1 :: cs' => if c = c1 then cs' else c1 :: remove_card (cs', c, e)

fun all_same_color (cards) =
  case cards of
       [] => true
     | c1 :: [] => true
     | c1 :: c2 :: cards' => card_color c1 = card_color c2 andalso
       all_same_color cards'

fun sum_cards (cards) =
let
  fun aux (cards, sum) =
    case cards of
         [] => sum
       | c :: cards' => aux (cards', card_value c + sum)
in
  aux (cards, 0)
end

fun score (held_cards, goal) =
let
  val sum = sum_cards held_cards
  val preliminary_score =  if sum > goal then 3 * (sum - goal) else goal - sum
in
  if all_same_color held_cards
  then preliminary_score div 2
  else preliminary_score
end

fun officiate (cards, moves, goal) =
let
  fun play_game (cards, held_cards, moves, goal) =
    case (cards, moves) of
         (_, []) => score (held_cards, goal)
       | ([], _) => score (held_cards, goal)
       | (c::cards', Draw::moves') => if sum_cards held_cards > goal
                                      then score (held_cards, goal)
                                      else play_game (cards', c::held_cards,
                                      moves', goal)
       | (cards, Discard c::moves') => play_game (cards, remove_card
       (held_cards, c, IllegalMove), moves', goal)
in
  play_game (cards, [], moves, goal)
end
