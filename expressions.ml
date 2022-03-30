module Expressions = struct 
  (* Defines Boolean expressions over primitive tests of type 't. *)
  type 't bool_exp = 
    | False
    | True
    | Test of 't
    | And of ('t bool_exp) * ('t bool_exp)
    | Or of ('t bool_exp) * ('t bool_exp)
    | Not of ('t bool_exp)

  (* Defines GKAT expressions over primitive tests of type 't and primitive actions of type 'a. *)
  type ('t, 'a) gkat_exp =
    | Do of 'a
    | Assert of ('t bool_exp)
    | Comp of (('t, 'a) gkat_exp) * (('t, 'a) gkat_exp)
    | Ife of (('t, 'a) gkat_exp) * ('t bool_exp) * (('t, 'a) gkat_exp)
    | While of (('t, 'a) gkat_exp) * ('t bool_exp)

  (* Defines KAT expressions over primitive tests of type 't and primitive actions of type 'a. *)
  type ('t, 'a) kat_exp =
    | Do of 'a
    | Assert of ('t bool_exp)
    | Comp of (('t, 'a) kat_exp) * (('t, 'a) kat_exp)
    | Plus of (('t, 'a) kat_exp) * (('t, 'a) kat_exp)
    | Star of (('t, 'a) kat_exp)

  (* Embeds GKAT expressions into KAT expressions. *)
  let rec embed_exp (e: ('t, 'a) gkat_exp): ('t, 'a) kat_exp =
    match e with
    | Do p -> Do p
    | Assert b -> Assert b
    | Comp (f, g) -> Comp (embed_exp f, embed_exp g)
    | Ife (f, b, g) -> Plus (Comp (Assert b, embed_exp f), Comp (Assert (Not b), embed_exp g))
    | While (f, b) -> Comp (Star (Comp (Assert b, embed_exp f)), Assert (Not b))

  (* Turns a Boolean expression into a string. *)
  let rec bool_exp_to_string (e: 't bool_exp) (test_to_string: 't -> string): string = 
    match e with 
    | False -> "false"
    | True -> "true"
    | Test t -> test_to_string t
    | And (f, g) -> "(" ^ (bool_exp_to_string f test_to_string) ^ " and " ^ (bool_exp_to_string g test_to_string) ^ ")"
    | Or (f, g) -> "(" ^ (bool_exp_to_string f test_to_string) ^ " or " ^ (bool_exp_to_string g test_to_string) ^ ")"
    | Not f -> "-(" ^ (bool_exp_to_string f test_to_string) ^ ")"

  (* Turns a KAT expression into a string. *)
  let rec kat_exp_to_string (e: ('t, 'a) kat_exp) (test_to_string: 't -> string) (action_to_string: 'a -> string): string =
    match e with 
    | Do a -> action_to_string a
    | Assert b -> bool_exp_to_string b test_to_string
    | Comp (f, g) -> (kat_exp_to_string f test_to_string action_to_string) ^ ";" ^ (kat_exp_to_string g test_to_string action_to_string)
    | Plus (f, g) -> "(" ^ (kat_exp_to_string f test_to_string action_to_string) ^ "+" ^ (kat_exp_to_string g test_to_string action_to_string) ^ ")"
    | Star f -> "(" ^ (kat_exp_to_string f test_to_string action_to_string) ^ ")^*"  

  (* Computes the complement of an atom. *)
  let complement (alpha: 't list) (tests: 't list) : 't list =
    let rec f (todo: 't list) (xs: 't list) : 't list =
      match todo with 
      | [] -> xs
      | hd :: tl ->
        if not (List.mem hd alpha) then
          f tl (hd :: xs)
        else
          f tl xs
    in f tests []

  (* Checks whether an atom alpha implies a Boolean expression b. *)
  let rec entails (alpha: 't list) (b: 't bool_exp) (tests: 't list) : bool =
    match b with 
    | False -> false
    | True -> true
    | Test t -> (List.mem t alpha)
    | And (c, d) -> (entails alpha c tests) && (entails alpha d tests)
    | Or (c, d) -> (entails alpha c tests) || (entails alpha d tests)
    | Not c -> entails (complement alpha tests) c tests
end
