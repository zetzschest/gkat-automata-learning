module Test = struct  
  open Expressions 
  open Extra
  open Automata
  open Oracle
  open Table 
  open Table2

  type word = Table2.word
  type ('a, 'b) coproduct = ('a, 'b) Automata.coproduct
  type one = Automata.one

  module Wordopt : Set.OrderedType with type t = word option = struct
    type t = word option 
    
    let compare x y = 
      Stdlib.compare x y
  end

  module Wordopt_One : Set.OrderedType with type t = (word option, one) coproduct  = struct
    type t = (word option, one) coproduct 
    
    let compare x y = 
      Stdlib.compare x y
  end
  
  module O = OracleExp(Wordopt)
  module OEmb = OracleExp(Wordopt_One)

  (* Constructs an oracle for GLstar from a KAT expression. *)
  let oracle_of_exp_gkat (e: (int, int) Expressions.kat_exp) (number_of_tests: int) (number_of_actions: int) : ((word option, one) coproduct, ((int list) * int), int list -> bool) Oracle.oracle =
    OEmb.oracle_from_kat_exp e number_of_tests number_of_actions (fun x -> x)

  (* Constructs an oracle for Lstar from a KAT expression. *)
  let oracle_of_exp (e: (int, int) Expressions.kat_exp) (number_of_tests: int) (number_of_actions: int) : (word option, ((int list) * int), int list -> bool) Oracle.oracle =
    O.oracle_from_kat_exp e number_of_tests number_of_actions (fun x -> x)

  let test (e: (int, int) Expressions.gkat_exp) (max_number_of_tests: int) (number_of_actions: int) =
    let rec f (test_counter: int) (current_string) = 
      if test_counter <= max_number_of_tests then
        let e_kat : ((int, int) Expressions.kat_exp) = Expressions.embed_exp e in 
        let oracle_kat = oracle_of_exp e_kat test_counter number_of_actions in 
        let oracle_gkat = oracle_of_exp_gkat e_kat test_counter number_of_actions in 
        let glstar_output_autom_tuple = Table2.glstar oracle_gkat.mq oracle_gkat.eq test_counter number_of_actions in 
        let lstar_output_autom_tuple = Table.lstar_moore oracle_kat.mq oracle_kat.eq test_counter number_of_actions in 
        let number_of_mq_gkat = snd glstar_output_autom_tuple in 
        let number_of_mq_kat = snd lstar_output_autom_tuple in 
        let new_string = current_string ^ "actions: " ^ (string_of_int number_of_actions) ^ ", tests: " ^ (string_of_int test_counter) ^ ", mq_glstar: " ^ (string_of_int number_of_mq_gkat) ^ ", mq_lstar: " ^ (string_of_int number_of_mq_kat) ^ "\n" in 
        f (test_counter + 1) new_string
      else
        let oc = open_out ("number_of_queries.csv") in
        Printf.fprintf oc "%s" current_string;
        close_out oc;  
    in f 1 ""     
end

open Expressions

module Test2 = struct
  let e : ((int, int) Expressions.gkat_exp) = Comp(While (Do 1, Test 1), Do 2) in 
  (* let e : ((int, int) Expressions.gkat_exp) = Ife(Do 1, Test 1, Do 2) in  *)
  let max_number_of_tests = 9 in
  let number_of_actions = 2 in 
  (* let number_of_actions = 3 in  *)
  Test.test e max_number_of_tests number_of_actions;
end



