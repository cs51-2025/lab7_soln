(*
                              CS51 Lab 7
                          Modules & Functors

A simple interface for the values and types of color.ml. You should
not change this file.  
 *)

type color ;;

type color_name =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Indigo
  | Violet ;;

(* to_color r g b -- Returns the `color` corresponding to the RGB
   values given by `r`, `g`, and `b` *)
val to_color : int -> int -> int -> color ;;

(* red_channel c / green_channel c / blue_channel c -- Returns the
   corresponding channel value for the color `c` *)
val red_channel : color -> int ;;
val green_channel : color -> int ;;
val blue_channel : color -> int ;;

(* color_named name -- Returns the `color` corresponding to the color
   `name` *)
val color_named : color_name -> color ;;
