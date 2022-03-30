module Automata = struct
  (* Defines a Moore coalgebra with state-space of type 'x, input of type 'a, and output of type 'b in the standard way. *)
  type ('x, 'a, 'b) moore_coalg = {
    delta: 'x -> 'a -> 'x;
    eps: 'x -> 'b;
  }

  (* A Moore automaton is a Moore coalgebra with initial state. *)
  type ('x, 'a, 'b) moore_autom = {
    initial: 'x;
    transition: ('x, 'a, 'b) moore_coalg;
  }

  (* An atom alpha of type 't can be identified with a list of primitive tests of type 't which are true in alpha. *)
  type 't atom = 't list

  (* A KAT coalgebra is a particular Moore coalgebra. *)
  type ('x, 't, 'a) kat_coalg = ('x, 't atom * 'a, 't atom -> bool) moore_coalg

  (* A KAT automaton is a KAT coalgebra with initial state. *)
  type ('x, 't, 'a) kat_autom = ('x, 't atom * 'a, 't atom -> bool) moore_autom

  (* Defines the categorical coproduct. *)
  type ('a, 'b) coproduct = Left of 'a | Right of 'b

  (* Defines a GKAT coalgebra over primitive tests of type 't and primitive actions of type 'a in the standard way. *)
  type ('x, 't, 'a) gkat_coalg = {
    delta: 'x -> 't atom -> (bool, 'a * 'x) coproduct;
  }

  (* A GKAT automaton is a GKAT coalgebra with initial state. *)
  type ('x, 't, 'a) gkat_autom = {
    initial: 'x;
    transition: ('x, 't, 'a) gkat_coalg;
  }

  (* Defines a word over an alphabet 'a as a list over 'a. *)
  type 'a word = 'a list

  (* Defines the categorial one, i.e. singleton set. *)
  type one = One

  (* Assigns to a GKAT automaton a language equivalent KAT automaton with sink state. *)
  let embed (autom: ('x, 't, 'a) gkat_autom) : (('x, one) coproduct, 't, 'a) kat_autom = 
      let delta' (x: ('x, one) coproduct) (prod: 't atom * 'a): ('x, one) coproduct = 
        let alpha = fst prod in 
        let p = snd prod in 
        match x with 
        | Left y -> (match autom.transition.delta y alpha with | Right (q, z) -> if p = q then Left z else Right One | Left _ -> Right One)  
        | Right One -> Right One 
      in 
      let eps' (x: ('x, one) coproduct) (alpha: 't atom): bool = 
        match x with
        | Left y -> (match autom.transition.delta y alpha with | Left true -> true | _ -> false) 
        | Right One -> false
      in 
      { initial = Left autom.initial; transition = { delta = delta'; eps = eps'; }; }
  
  (* Computes the state of a Moore automaton reached by a word. *)
  let reach_state (autom: ('x, 'a, 'b) moore_autom) (w: 'a word) : 'x =
    let rec f (state: 'x) (todo: 'a word) : 'x =
      match todo with 
      | [] -> state
      | hd :: tl -> f (autom.transition.delta state hd) tl 
    in f autom.initial w 

  (* Computes the output of the state of a Moore automaton that is reached by a word. *)
  let reach (autom: ('x, 'a, 'b) moore_autom) (w: 'a word): 'b = 
    autom.transition.eps (reach_state autom w)  
  
  open Expressions    

  (* Given a KAT expression e, computes in the standard way via the Brzozowski-derivatives a KAT automaton that is language equivalent to e. *)
  let deriv (e: ('t, 'a) Expressions.kat_exp) (tests: 't list) : (('t, 'a) Expressions.kat_exp, 't, 'a) kat_autom =
    let rec eps' (x: ('t, 'a) Expressions.kat_exp) (alpha: 't atom) : bool =
      match x with 
      | Do _ -> false
      | Assert b -> Expressions.entails alpha b tests
      | Comp (f, g) -> if (eps' f alpha) then (eps' g alpha) else false
      | Plus (f, g) -> if (eps' f alpha) then true else (eps' g alpha)
      | Star _ -> true 
    in 
    let rec delta' (x: ('t, 'a) Expressions.kat_exp) (tuple: 't atom * 'a) : ('t, 'a) Expressions.kat_exp =
      let alpha = fst tuple in
      let p = snd tuple in
      match x with
      | Do q -> if p == q then (Assert True) else (Assert False)
      | Assert _ -> Assert False
      | Comp (f, g) -> 
        if eps' f alpha then 
          (match delta' f (alpha, p) with
          | Assert True -> 
            (match delta' g (alpha, p) with
            | Assert True -> Assert True
            | Assert False -> g 
            | _ -> Plus (g, delta' g (alpha, p)))
          | Assert False -> delta' g (alpha, p)
          | _ -> 
            (match delta' g (alpha, p) with 
            | Assert True -> Assert True
            | Assert False -> Comp (delta' f (alpha, p), g)
            | _ -> Plus (Comp (delta' f (alpha, p), g), delta' g (alpha, p))))
        else 
          (match delta' f (alpha, p) with 
          | Assert True -> g
          | Assert False -> Assert False
          | _ -> Comp (delta' f (alpha, p), g))
      | Plus (f, g) -> (match delta' f (alpha, p) with | Assert True -> Assert True | Assert False -> delta' g (alpha, p) | _ -> Plus (delta' f (alpha, p), delta' g (alpha, p)))
      | Star f -> (match delta' f (alpha, p) with | Assert True -> Star(f) | Assert False -> Assert False | _ -> Comp (delta' f (alpha, p), Star(f)))
    in { initial = e; transition = { delta = delta'; eps = eps'; }; }

  open Extra

  (* Uses "deriv" above for "number_of_tests" many primitive tests. *)
  let autom_from_kat_exp (e: (int, int) Expressions.kat_exp) (number_of_tests: int) : ((int, int) Expressions.kat_exp, int, int) kat_autom  =
    let tests = Extra.range_from_one(number_of_tests) in
    deriv e tests      
end

module Print (Ord: Set.OrderedType) = struct
  module M = Map.Make(Ord)

  open Automata

  (* Derives the data that is needed to print the reachable part of a Moore automaton. *)
  let walk (autom: ('x, 'a, 'b) moore_autom) (encode: 'x -> Ord.t) (alphabet: 'a list) = 
    let todo = Queue.create () in
    Queue.add autom.initial todo;
    let rec f seen_structure =   
      if Queue.is_empty todo then
        seen_structure
      else
        let x = Queue.pop todo in 
        if not (M.mem (encode x) seen_structure) then
            let rec create_trans (xs: 'a list) map = 
              match xs with 
              | [] -> map
              | hd :: tl -> 
                let next_state = autom.transition.delta x hd in
                Queue.add next_state todo;
                if M.mem (encode next_state) map then
                  let new_map = M.update (encode next_state) (Option.map (fun ys -> hd :: ys)) map in
                  create_trans tl new_map
                else
                  let new_map = M.add (encode next_state) [hd] map in 
                  create_trans tl new_map
            in 
            let trans_at = create_trans alphabet M.empty in 
            let output_at = autom.transition.eps x in
            let new_seen_structure = M.add (encode x) (trans_at, output_at) seen_structure in 
            f new_seen_structure
          else
            f seen_structure     
    in f M.empty 

  (* Turns a Moore automaton into a (.dot compatible) string. *)
  let autom_to_string (autom: ('x, 'a, 'b) moore_autom) (encode: 'x -> Ord.t) (alphabet: 'a list) (to_string_a: 'a list -> string) (to_string_b: 'b -> string) (to_string_x: Ord.t -> string) : string = 
    let structure_list  = M.bindings (walk autom encode alphabet) in 
    let rec create_map xs m i = 
      match xs with
      | [] -> m 
      | hd :: tl -> 
          let state = fst hd in 
          let state_string = "S" ^ (string_of_int i) in
          create_map tl (M.add state state_string m) (i + 1)
    in 
    let state_strings = create_map structure_list M.empty 0 in 
    let state_string encoded_state = 
      M.find encoded_state state_strings
    in 
    let string_part_begin1 = "digraph G {\n  Initial [shape=point];\n" in 
    let string_part_begin2 = "  Initial -> " ^ (state_string (encode autom.initial)) ^ ";\n" in
    let string_end = "}" in 
    let rec f xs (str: string) =
      match xs with 
      | [] -> str
      | hd :: tl -> 
        let encoded_state = fst hd in
        let trans_at = fst (snd hd) in
        let trans_at_list = M.bindings trans_at in
        let output_at = snd (snd hd) in 
        let string_node = "  " ^ (state_string encoded_state)  ^ " [label=\"" ^ (to_string_x encoded_state) ^ "\", xlabel= \"{" ^ (to_string_b output_at) ^ "}\"];\n" in 
        let rec g xs' (str': string) =
          match xs' with 
          | [] -> str'
          | hd' :: tl' -> 
            let encoded_trans_state = fst hd' in 
            let list_of_input_chars = snd hd' in 
            let string_single_trans = "  " ^ (state_string encoded_state) ^ " -> " ^ (state_string encoded_trans_state) ^ " [label=\"" ^ (to_string_a list_of_input_chars) ^ "\"];\n" in 
            g tl' (str' ^ string_single_trans)
        in
        let string_trans = g trans_at_list "" in
        let add_to_string = string_node ^ string_trans in 
        f tl (str ^ add_to_string)
    in 
    let string_middle = f structure_list "" in
    string_part_begin1 ^ string_part_begin2 ^ string_middle ^ string_end 
end





