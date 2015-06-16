(* Jorge Conde
	V00723209

	CSC 330
	Assignment #2
	Jan 22, 2015
*)

fun same_string(s1 : string, s2 : string) =
    s1 = s2


fun all_except_option(s1, lst) =
	case lst of
		[] => NONE
	  | x::xs' => case same_string(s1, x) of
				true => SOME xs'
		  	  | false => case all_except_option(s1, xs') of
							NONE => NONE
						  | SOME y => SOME (x::y)

fun get_substitutions1(listoflist, s1) = 
	case listoflist of
		[] => []
	  | x::xs' => case all_except_option(s1, x) of
	  				NONE => get_substitutions1(xs', s1)
	  			  | SOME y => y @ get_substitutions1(xs', s1) 

fun get_substitutions2(listoflist, s1) =
	let 
		fun help_subsitution(listoflist, s1, acc) =
			case listoflist of	
				[] => acc
			  | x::xs' => case all_except_option(s1, x) of
			  				NONE => help_subsitution(xs', s1, acc)
			  			  | SOME y => help_subsitution(xs', s1, acc @ y)
	in
		help_subsitution(listoflist, s1, [])
	end


fun similar_names (lst, fullname) =
    let val {first=first,middle=m,last=l} = fullname
        fun generate_names(sublst) =
            case sublst of
                [] => []
              | x::xs' =>
                	{first=x,middle=m,last=l}::generate_names(xs')
    in
        fullname::generate_names(get_substitutions2(lst, first))
    end


(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove


fun card_color(s, r) =
	case s of
		Clubs => Black
	  | Diamonds => Red
	  | Hearts => Red
	  | Spades => Black

fun card_value(s, r) =
	case r of
		Jack => 10
	  | Queen => 10
	  |	King => 10
	  |	Ace => 11
	  | Num i => i

fun remove_card(cs, c, e) = 
	case cs of
		[] => raise e
	  | x::xs' => if x = c then xs' else x::remove_card(xs', c, e)

fun all_same_color(cs) =
	case cs of
		[] => true
	  | x::[] => true
	  | x::y::xs' => (card_color(x) = card_color(y)) andalso all_same_color(y::xs')

fun sum_cards(cs) =
	let 
		fun help_sum (cs, acc) =
			case cs of
				[] => acc
			  | x::xs' => help_sum(xs', card_value(x)+acc)
	in
		help_sum(cs, 0)
	end

fun score(hc, goal) = 
	let
		val sum = sum_cards(hc)
		val ps = if sum > goal then 2 * (sum - goal) else (goal - sum)
	in
		case all_same_color(hc) of
			true => ps div 2
		  | false => ps
	end


fun officiate(cs, moves, goal) =
	let
		fun play(hc, sub_cs, sub_moves ) = 
			case sub_moves of
				[] => hc
			  |	Discard x::xs' => play(remove_card(hc, x, IllegalMove), sub_cs, xs')
			  | Draw::xs' => case sub_cs of
			  					[] => hc
			  				  | c::c' =>
			  				  		if (card_value(c) + sum_cards(hc)) > goal
			  				  		then c::hc
			  				  		else play(c::hc, c', xs')
	in
		score(play([], cs, moves), goal)
	end

