(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml"; 

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")

(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

fun make_silly_json (i: int) =
    let fun maker (i: int) = 
	    case i of
		0 => []
	     |  _ => Object [("n", Num (int_to_real i)),("b", True)]::maker(i-1)
    in Array (maker(i))
    end

fun assoc (k: ''a, xs: (''a * 'b) list) =
    case xs of
	[] => NONE
      | (k1,v1)::xs' => if k1 = k then SOME v1 else assoc (k, xs')

fun dot (j: json, f: string) =
    case j of
	Object v => assoc(f, v)
      | _ => NONE

fun one_fields (j: json) =
    let
	fun helper (obj: (string * json) list, acc: string list) =
	    case obj of
		[] => acc
	      | (field,_)::next => helper (next, field::acc)
    in
    case j of
	Object object => helper (object, [])
      | _  => [] 
    end

fun no_repeats (ls: string list) =
    (length ls) = length (dedup ls)

fun recursive_no_field_repeats (j: json) =
    let fun object_helper (obj: (string * json) list)=
	    case obj of
		[] => true
	      | (_,x)::xs => recursive_no_field_repeats (x) andalso object_helper (xs)
	fun array_helper (arr: json list) =
	    case arr of
		[] => true
	      | x::xs => recursive_no_field_repeats (x) andalso array_helper (xs) 
    in
	case j of
	    Object object => no_repeats (one_fields(j)) andalso object_helper (object)
	  | Array arr => array_helper (arr) 
	  | _  => true 
    end

fun count_occurrences (ls:string list, excep: exn) =
    let fun count_helper (ls: string list, c_string: string, c_count: int, accum: (string * int) list, is_sorted: order) = 
	    case ls of
		[] => (c_string, c_count) :: accum
	      | x::xs => if x = c_string 
			 then count_helper(xs, c_string, c_count+1, accum, is_sorted) 
			 else if is_sorted = LESS
			 then raise excep
			 else count_helper(xs, x, 1, (c_string, c_count)::accum, strcmp(x, c_string))
    in
	case ls of 
	    [] => []
	  | x::xs => count_helper(xs, x, 1,[], EQUAL)
    end

fun string_values_for_field (s: string, jl: json list) =
    case jl of
	[] => []
      | x::xs => case dot(x, s) of
		     SOME (String v) => v::string_values_for_field(s, xs)
		   | _ => string_values_for_field (s,xs)
    
(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
    histogram (string_values_for_field (f, js))
	      
(**** PUT PROBLEMS 9-11 HERE ****)

fun filter_field_value (field_name: string, field_content: string, jl: json list) =
    case jl of
	[] => []
      | x::xs => case dot(x, field_name) of
		     SOME (String v) => if field_content = v
					then x::filter_field_value(field_name,field_content,xs)
					else filter_field_value(field_name,field_content,xs)
		  | _ => filter_field_value(field_name,field_content,xs)

val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description",large_incident_reports_list)

val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", large_incident_reports_list)

	 
;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)
val forty_third_and_the_ave_reports = filter_field_value ("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)
							 
val forty_third_and_the_ave_event_clearance_description_histogram =  histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports)
											
val nineteenth_and_forty_fifth_reports = filter_field_value ("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list)
							    
val nineteenth_and_forty_fifth_event_clearance_description_histogram  =  histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports)
											    
;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)
fun concat_with (separator: string, ls: string list) =
    case ls of
	[] => ""
      | x::xs => let val nextval = concat_with (separator, xs)
		 in if nextval = "" then x^nextval else x^separator^nextval end 
		     
fun quote_string (s) =
    "\""^s^"\""
	       
fun real_to_string_for_json (r: real) =
    if real_is_negative(r) then "-"^real_to_string (real_abs(r)) else real_to_string(r)
										    
fun json_to_string (j: json) =
    let fun array_helper (arr: json list) =
	    case arr of
		[] => ""	  
	      | x::xs => let val nextval = array_helper (xs)
			 in if nextval = ""
			    then json_to_string(x)^nextval
			    else json_to_string(x)^", "^nextval
			 end
	fun object_helper (obj: (string * json) list) =
	    case obj of
		[] => ""
	      | (f,c)::xs => let val nextval = object_helper(xs)
			     in if nextval = ""
				then concat_with(" : ",[quote_string(f),json_to_string(c)])^nextval
				else concat_with(" : ",[quote_string(f),json_to_string(c)])^", "^nextval
			     end
    in									  
	case j of
	    Num r => real_to_string_for_json (r)
	  | False => "false"
	  | True => "true"
	  | Null => "null"
	  | String s => quote_string (s)
	  | Array arr => concat_with(array_helper (arr), ["[","]"])
	  | Object obj => concat_with(object_helper (obj), ["{","}"])
    end
(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

