(* Assign 03 Provided Code *)

(*  Version 1.0 *)

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


fun only_capitals(xs) = 
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs
 
fun longest_string1(xs) = 
    foldl (fn (x,acc) => if String.size x > String.size acc then x else acc) "" xs
 
fun longest_string2(xs) = 
    foldl (fn (x,acc) => if String.size x >= String.size acc then x else acc) "" xs
 
fun longest_string_helper f xs = 
    foldl (fn (x,acc) => if f(String.size x, String.size acc) then x else acc) "" xs
 
val longest_string3 = 
    longest_string_helper (fn (x,y) => x > y)
 
val longest_string4 = 
    longest_string_helper (fn (x,y) => x >= y)
 
val longest_capitalized =
    longest_string3 o only_capitals
 
val rev_string = 
    String.implode o rev o String.explode
 
 
fun first_answer f [] = raise NoAnswer
  | first_answer f (x :: xs') = case f x of 
				    SOME x' => x'
				  | NONE => first_answer f xs'
 
fun all_answers f [] = SOME []
  | all_answers f xs = 
    let fun loop(acc, []) = SOME acc
	  | loop(acc, SOME(y)::ys) = loop(acc @ y, ys)
	  | loop(acc, NONE::ys) = NONE
    in
	loop([], map f xs)
    end	
	
fun count_wildcards(p) = 
    g (fn () => 1) (fn x => 0) p
 
fun count_wild_and_variable_lengths(p) =
    g (fn () => 1) (fn x => String.size x) p
 
fun count_some_var(s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p
 
fun check_pat(p) = 
    let fun list_vars (Variable x) = [x]
	  | list_vars (TupleP ps) = List.foldl (fn (p', acc) => acc @ list_vars(p')) [] ps
	  | list_vars (_) = []
	fun has_repeats ([]) = false
	  | has_repeats (x::xs) = List.exists (fn x' => x = x') xs orelse has_repeats xs
    in
	(not o has_repeats o list_vars) p
    end
 
fun match(v, p) = 
    case (p, v) of
	(Wildcard, _) => SOME []
      | (Variable s, _) => SOME [(s,v)]
      | (UnitP, Unit) => SOME []
      | (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs 
				 then all_answers (fn (vs',ps') => match(vs',ps')) (ListPair.zip(vs,ps))
				 else NONE
      | (ConstructorP(s1,pp), Constructor(s2,pv)) => if s1 = s2 then match(pv,pp) else NONE
      | _ => NONE
 
fun first_match v ps = 
    ( SOME(first_answer (fn p => match(v,p)) ps) ) handle NoAnswer => NONE
