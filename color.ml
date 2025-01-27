(*
                              CS51 Lab 7
                          Modules & Functors

                 A module for colors and color names

The representation for colors in this implementation of the signature in
`color.mli` is really obscure (and arguably unnecessarily so). By the
way, it also has some bugs so it doesn't pass all the unit tests. No
need to debug it though, or even read it. You'll be replacing it
wholesale with your own hopefully simpler implementation. But you'll
*definitely* want to look over the `color.mli` signature. *)

(*
                               SOLUTION

In this implementation, we simply use int triples for the RGB
channels. By keeping the data in a more structured form, all of the
conversions are simplified.

You might have used a record type instead of a tuple, for instance,

    type color = { red : int; green : int; blue : int } ;;

That could work too. And the user of the module (the file
`lab7_part2.ml`) *wouldn't be able to tell the difference*, just like it
can't tell the difference with the horribly obfuscated implementation in
the original version of `color.ml`.

You'll notice that we've moved all of the documentation on module
elements (types, functions, and the like) into the `.mli` file, since
it's the signature file that codifies what users of the module "need to
know". *)

type color = int * int * int ;;

type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet ;;

let to_color (r : int) (g : int) (b : int) : color =
  r, g, b ;;

let red_channel   ( r, _g, _b : color) : int = r ;;
let green_channel (_r,  g, _b : color) : int = g ;;
let blue_channel  (_r, _g,  b : color) : int = b ;;

let color_named (name : color_name) : color =
  match name with
  | Red    -> to_color 255    0    0
  | Green  -> to_color   0  255    0
  | Blue   -> to_color   0    0  255
  | Orange -> to_color 255  165    0
  | Yellow -> to_color 255  255    0
  | Indigo -> to_color  75    0  130
  | Violet -> to_color 240  130  240
