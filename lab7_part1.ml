(*
                              CS51 Lab 7
                   Modules and Abstract Data Types
 *)

(*
                               SOLUTION
 *)

(* Objective: This lab practices concepts of modules, including files
as modules, signatures, and polymorphic abstract data types.

There are three total parts to this lab. Please refer to the following
files to complete all exercises:

-> lab7_part1.ml -- Part 1: Implementing modules (this file)
   lab7_part2.ml -- Part 2: Files as modules
   lab7_part3.ml -- Part 3: Polymorphic abstract types and
                            Interfaces as abstraction barriers
 *)

(*======================================================================
Part 1: Implementing Modules

*Modules* are a way to package together and encapsulate types and values
(including functions) into a single discrete unit.

By applying a *signature* to a module, we guarantee that the module
implements at least the values and functions defined within it. The
module may also implement more as well, for internal use, but only those
specified in the signature will be exposed and available outside the
module definition. This form of abstraction, information hiding,
implements the edict of compartmentalization.

In this part, you'll revisit the "weather" example from lab 6 part
1. Recall that in that lab, you defined some algebraic data types for
representing aspects of the season and weather, along with some
functions to extract the precipitation amount and to generate a string
description of the weather. We can define a module signature for the
types and functions from that lab as follows:
 *)

module type WEATHER = sig
  type season = Spring | Summer | Autumn | Winter
                                               
  type condition = 
    | Sunny
    | Rainy of int  (* precipitation in mm *)
    | Snowy of int  (* precipitation in mm *)
                 
  type weather_status = {season : season; condition : condition}
                          
  val describe_weather : weather_status -> string
  val precipitation_amount : condition -> int
end ;;

(* Notice that we've left out the function `season_to_string` from the
signature, since it was really just a helper function for
`describe_weather`. There's no reason that users of the module should
need to use this function. *)

(*......................................................................
Exercise 1A: Complete the implementation of a module called `Weather`
that satisfies the signature above. Feel free to make use of your
solution or the staff solution for lab 6 part 1.
......................................................................*)

module Weather : WEATHER = struct
  type season = Spring | Summer | Autumn | Winter
                                               
  type condition = 
    | Sunny
    | Rainy of int
    | Snowy of int
                 
  type weather_status = {season : season; condition : condition}
                          
  (* We still make use of the `season_to_string` helper function. It
     won't be exposed in the interface. *)
  let season_to_string (season : season) : string =
    match season with
    | Spring -> "spring"
    | Summer -> "summer"
    | Autumn -> "autumn"
    | Winter -> "winter"

  let precipitation_amount (condition : condition) : int =
    match condition with
    | Sunny -> 0
    | Rainy amount | Snowy amount -> amount
                                       
  let describe_weather { season; condition } =
    let season_str = season_to_string season in
    let amount = precipitation_amount condition in
    let common_str = " in " ^ season_str
                     ^ ". Precipitation: "
                     ^ string_of_int amount ^ " mm." in
    match condition with
    | Sunny   -> "It's a sunny " ^ season_str ^ " day."
    | Rainy _ -> "It's raining" ^ common_str
    | Snowy _ -> "It's snowing" ^ common_str
end ;;

(*......................................................................
Exercise 1B: Now that you've implemented the `Weather` module, use it
to generate a string description of a rainy winter day with 20 mm of
rain. That is, define a value `example : string` that uses
the `Weather` module to generate its string value

    "It's raining in winter. Precipitation: 20 mm."

(Use explicit module prefixes for this exercise, not global or local
opens.)
......................................................................*)

let example =
  Weather.describe_weather
    {Weather.season = Weather.Winter;
     Weather.condition = Weather.Rainy 20}  ;;

(* You may find that, strictly speaking, the `Weather.` module
   qualifiers are not needed on the field names (like `season`) and
   value constructors (like `Sunny`). That's because OCaml may manage
   to infer which `season` or `Sunny` you may be referring to. But
   OCaml can't always reconstruct them, so we've included them for
   explicitness, and to highlight the contrast in the next
   exercise. *)

(*......................................................................
Exercise 1C: Reimplement `example` from 1B above, now as
`example_local_open`, but using a "local open" to write your
computation in a more succinct manner.
......................................................................*)

let example_local_open =
  let open Weather in
  describe_weather {season = Winter; condition = Rainy 20} ;;
