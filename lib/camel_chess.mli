module Board : module type of Board
(** Core board model and setup operations. *)

module Render : module type of Render
(** Terminal renderer for Camel Chess. *)

module Logic : module type of Logic
(** Command parsing and gameplay logic helpers. *)
