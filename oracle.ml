type 'a bisimul_return = 'a Bisimulation.bisimul_return
type 'a word = 'a list

module Oracle = struct
  open Automata 

  type ('x, 'a, 'b) oracle = {
    mq: 'a word -> 'b;
    eq: ('x, 'a, 'b) Automata.moore_autom -> 'a bisimul_return
  } 

end

module Oracle2 (Ord1: Set.OrderedType) (Ord2: Set.OrderedType) = struct
  open Automata
  open Oracle
  module B = Bisimulation.Bisimulation(Ord1)(Ord2)

  (* Constructs an oracle from a Moore automaton: equivalence queries are answered via bisimulation and membership queries by reachability. *)
  let oracle_from_autom (autom1: ('x, 'a, 'b) Automata.moore_autom) (f1: 'x -> Ord1.t) (f2: 'y -> Ord2.t) (alphabet: 'a list) (equal_b: 'b -> 'b -> bool) : ('y, 'a, 'b) oracle =
    let mq' (w: 'a word) : 'b =
      Automata.reach autom1 w
    in 
    let eq' (autom2: ('y, 'a, 'b) Automata.moore_autom) : 'a bisimul_return =
      B.bisimulation autom1 autom2 f1 f2 alphabet equal_b
    in 
    { mq = mq'; eq = eq'; }
end

module OracleExp (Ord: Set.OrderedType) = struct
  open Expressions
  open Extra
  open Oracle
  open Automata

  module IntExp : Set.OrderedType with type t = ((int, int) Expressions.kat_exp) = struct
    type t = (int, int) Expressions.kat_exp
  
    let compare x y = 
      Stdlib.compare x y
  end

  module O = Oracle2(IntExp)(Ord) 

  (* Constructs an oracle from a KAT expression, using the KAT automaton it induces. *)
  let oracle_from_kat_exp (e: (int, int) Expressions.kat_exp) (number_of_tests: int) (number_of_actions: int) (f: 'y -> Ord.t) : ('y, ((int list) * int), int list -> bool) oracle =
    let tests = Extra.range_from_one(number_of_tests) in
    let actions = Extra.range_from_one(number_of_actions) in 
    let alphabet = Extra.from_tests_and_actions_to_atoms_and_actions tests actions in
    let atoms = Extra.from_tests_to_atoms tests in 
    let autom = Automata.deriv e tests in
    let equal (f: 'a -> bool) (g: 'a -> bool) (ys: 'a list) : bool =
      let f xs =
        match xs with
        | [] -> true
        | hd :: tl -> (f hd = g hd)
      in f ys
    in 
    let equal_b (f: int list -> bool) (g: int list -> bool) : bool =
      equal f g atoms
    in 
    O.oracle_from_autom autom (fun x -> x) f alphabet equal_b
end