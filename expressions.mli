module Expressions :
  sig
    type 't bool_exp =
        False
      | True
      | Test of 't
      | And of 't bool_exp * 't bool_exp
      | Or of 't bool_exp * 't bool_exp
      | Not of 't bool_exp
    type ('t, 'a) gkat_exp =
        Do of 'a
      | Assert of 't bool_exp
      | Comp of ('t, 'a) gkat_exp * ('t, 'a) gkat_exp
      | Ife of ('t, 'a) gkat_exp * 't bool_exp * ('t, 'a) gkat_exp
      | While of ('t, 'a) gkat_exp * 't bool_exp
    type ('t, 'a) kat_exp =
        Do of 'a
      | Assert of 't bool_exp
      | Comp of ('t, 'a) kat_exp * ('t, 'a) kat_exp
      | Plus of ('t, 'a) kat_exp * ('t, 'a) kat_exp
      | Star of ('t, 'a) kat_exp
    val embed_exp : ('t, 'a) gkat_exp -> ('t, 'a) kat_exp
    val bool_exp_to_string : 't bool_exp -> ('t -> string) -> string
    val kat_exp_to_string :
      ('t, 'a) kat_exp -> ('t -> string) -> ('a -> string) -> string
    val complement : 't list -> 't list -> 't list
    val entails : 't list -> 't bool_exp -> 't list -> bool
  end
