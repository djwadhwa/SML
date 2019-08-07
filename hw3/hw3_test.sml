use "hw3.sml";

val test1 = only_lowercase ["Hello", "hi", "bye", "Goodbye"] = ["hi", "bye"]

val long_string = ["Hello", "hi", "bye", "12345"]

val test2 = longest_string1 long_string = "Hello"
						
val test2a = longest_string1 [] = ""
						
val test3 = longest_string2 long_string = "12345"

val test4a = longest_string3 long_string = "Hello"
						
val test4b = longest_string4 long_string = "12345"
					
val test5 = longest_lowercase long_string = "bye"

val test5a = longest_lowercase ["Hello", "Hi"] = ""
					       
val test6 = caps_no_X_string "aBxXXxDdx" = "ABDD"

fun alpha_to_beta_option a =
    if a < 10
    then SOME a
    else NONE

val test7a = first_answer alpha_to_beta_option [10,9,8] = 9
							      	      
val test7b = (first_answer alpha_to_beta_option [10,11,12]
	      handle NoAnswer => ~1) = ~1

val test8a = all_answers (fn x=> SOME []) long_string = SOME []

val test8b = all_answers (fn x => NONE) long_string = NONE

val test8c = all_answers (fn x => if (x = "Hello" orelse x = "hi") then SOME [x] else SOME []) long_string = SOME ["hi", "Hello"]

val test9a = count_wildcards (TupleP [WildcardP, TupleP [WildcardP], WildcardP])= 3

val test9b = count_wild_and_variable_lengths (TupleP [WildcardP, WildcardP, VariableP "Hello", VariableP "Hi"]) = 9

val test9c = count_a_var ("Hi", TupleP [WildcardP, WildcardP, VariableP "Hello", VariableP "Hi"]) = 1

val test10a = check_pat (TupleP [VariableP "Hello", VariableP "Hi"]) = true

val test10b = check_pat (TupleP [VariableP "Hello", VariableP "Hello"]) = false

val test11a = match (Constant 17, ConstantP 17) = SOME []

val test11b = match (Constructor ("name", Unit), ConstructorP ("name", WildcardP)) = SOME []
						       
val test11c = match (Tuple [Unit],TupleP [WildcardP, WildcardP, VariableP "Hello", VariableP "Hi"]) = NONE
											
val test11d = match (Constant 12, UnitP) = NONE
						       
val test11e = match (Constant 17, WildcardP) = SOME []
						       
val test11f = match (Constant 17, VariableP "name") = SOME [("name", Constant 17)]
						       
    
