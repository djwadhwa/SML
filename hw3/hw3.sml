(* CSE341, HW3 Provided Code *)
exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)

fun only_lowercase (ls: string list) =
    List.filter (fn x => Char.isLower(String.sub(x, 0))) ls

fun longest_string1 (ls: string list) =
    List.foldl (fn (x,y) =>if String.size x > String.size y then x else y) "" ls
				
fun longest_string2 (ls: string list) =
    List.foldl (fn (x,y) =>if String.size x < String.size y then y else x) "" ls

	       (*(((int * int -> bool) -> string list) -> string) *)
	       (* ((func -> string list)-> string)*)
	       (* (func2->string) *)
fun longest_string_helper f ls  = List.foldl (fn (x,y) => if f(String.size x,String.size y) then x else y) "" ls

val longest_string3 = longest_string_helper (fn (x,y) =>  x > y)

val longest_string4 = longest_string_helper (fn (x,y) =>  y <= x )

val longest_lowercase = List.foldl (fn (x,y) =>if String.size x > String.size y then x else y) "" o only_lowercase 

val caps_no_X_string = String.implode o List.map (fn x => Char.toUpper x) o List.filter (fn x => not (x = #"x" orelse x = #"X")) o String.explode
											
fun first_answer f ls =
    case ls of
	
	[] => raise NoAnswer
      | x::xs => case f(x) of
		     SOME v => v 
		   | _ => first_answer f xs 
				       
					 
fun all_answers f ls =
    let fun helper (f, ls', acc) =
	    case ls' of
                [] => SOME acc
              | x::xs => case f(x) of
                             NONE => NONE
                           | SOME v => helper(f, xs, v@acc)
    in
       helper(f, ls, [])
    end

(* function g takes in 3 arguments, the first being function f1 which is called everytime a wildcard pattern is matched. The second argument is f2 which is called when a variable pattern is matched and is applied to the variable. The thrid argument is the pattern itself, so the function knows when function to call by matching to the right contructor*)
fun count_wildcards (p: pattern)= g (fn x => 1) (fn x=> 0) p

fun count_wild_and_variable_lengths (p: pattern) = g (fn x => 1) (fn x => String.size x) p

fun count_a_var (s: string, p: pattern) = g (fn x => 0) (fn x => if s=x then 1 else 0) p

fun check_pat (p: pattern) =
    let fun check_string_list ls = (* return true if list has unique string *)
	    case ls of
		[] => true 
	      | x::xs => not (List.exists (fn element => x = element) xs) andalso check_string_list xs
	fun var_to_string_list p =
	    case p of
		VariableP v => [v]
	      | TupleP pl => List.foldl (fn (v1,v2) => v2 @ var_to_string_list (v1)) [] pl
	      | ConstructorP (s,p') => var_to_string_list p'
	      | _ => []
    in
	check_string_list (var_to_string_list p)
end

fun match (v: valu, p: pattern) =
    case p of
	ConstantP num => (case v of
			      Constant n => if num = n
					    then SOME []
					    else NONE
			    | _ => NONE)
      | ConstructorP (s1,p') => (case v of
				     Constructor (s2, v') => if s1 = s2
							     then match(v',p')
							     else NONE
				   | _ => NONE)
      | TupleP pl => (case v of
			  Tuple vl => if length pl = length vl
				      then all_answers match (ListPair.zip(vl,pl))
				      else NONE
			| _ => NONE)
      | UnitP => (case v of
		      Unit => SOME []
		    | _ => NONE)
      | VariableP s => SOME [(s,v)]
      | WildCardP  => SOME []
			   
fun first_match (v: valu, pl: pattern list) =
    SOME (first_answer (fn p=> match (v,p)) pl) handle NoAnswer => NONE
