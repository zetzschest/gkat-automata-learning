type 'a bisimul_return = True | Counterexample of ('a list)
type 'a word = 'a list

module Bisimulation (Ord1: Set.OrderedType) (Ord2: Set.OrderedType) = struct
  (* Defines a functor that assigns to two modules of ordered type a module of lexigraphically ordered type. *)
  module Lexicographic (S1: Set.OrderedType) (S2: Set.OrderedType) : (Set.OrderedType with type t = S1.t * S2.t) = struct
    type t = S1.t * S2.t
    let compare (x1,y1) (x2,y2) =
      match S1.compare x1 x2 with
      | 0 -> S2.compare y1 y2
      | c -> c  
  end

  module R = Set.Make(Lexicographic(Ord1)(Ord2))

  (* Assigns a string to the result of a bisimulation calculation. *)
  let return_to_string (ret: 'a bisimul_return) (f: 'a word -> string) : string = 
    match ret with 
    | True -> "true"
    | Counterexample w -> f w
  
  open Automata 
  
  (* 
   Tries to construct a bisimulation between two Moore automata with same input and output in the standard way. 
   The calculation either succesfully yields a bisimulation, or provides a counterexample. 
  *)
  let bisimulation (autom1: ('x, 'a, 'b) Automata.moore_autom) (autom2: ('y, 'a, 'b) Automata.moore_autom) (f1: 'x -> Ord1.t) (f2: 'y -> Ord2.t) (alphabet: 'a list) (equal_b: 'b -> 'b -> bool): 'a bisimul_return =
    let todo = Queue.create () in 
    Queue.add ([], autom1.initial, autom2.initial) todo;
    let rec f (rel: R.t): 'a bisimul_return = 
      if Queue.is_empty todo then
        True
      else
        let (word, x, y) = Queue.pop todo in
        if not (R.mem (f1 x, f2 y) rel) then 
          if not (equal_b (autom1.transition.eps x) (autom2.transition.eps y)) then
            Counterexample word
          else
            let g a = 
              Queue.push (word @ [a], autom1.transition.delta x a, autom2.transition.delta y a) todo; 
            in 
            List.iter g alphabet;
            f (R.add (f1 x, f2 y) rel)
        else
          f rel
    in f R.empty
end