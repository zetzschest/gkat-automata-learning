# Guarded Kleene Algebra with Tests: Automata Learning
> Guarded Kleene Algebra with Tests (GKAT) is the fragment of Kleene Algebra with Tests (KAT) that arises by replacing the union and iteration operations of KAT with predicate-guarded variants. GKAT is more efficiently decidable than KAT and expressive enough to model simple imperative programs, making it attractive for applications to e.g. network verification. In this paper, we further explore GKAT’s automata theory, and present GL∗, an algorithm for learning the GKAT automaton representation of a black-box, by observing its behaviour. A complexity analysis shows that it is more efficient to learn a representation of a GKAT program with GL∗ than with Angluin’s existing L∗ algorithm. We implement GL∗ and L∗ in OCaml and compare their performances on example programs.

## About this repository

This repository allows one to compare, for any GKAT expression e, the number of membership queries to its language semantics [e] required by GL∗  for learning a GKAT automaton representation of *e*, with the number of membership queries to [e] required by L∗ for learning a Moore automaton representation of e. For each run, we output, for both algorithms, a trace of the involved hypotheses as tables in the .csv format and graphs in the .dot format, as well as an overview of the numbers of involved queries in the .csv format. 

### Example 1 

In [ife_results](ife_results) we present the results for the expression e =  **if** t1 **then** p1 **else** p2, the primitive actions {p1, p2, p3}, and primitive tests {t1, ..., tn} parametric in n = 1,...,9. The traces for GL∗ can be found in [ife_results/glstar/](ife_results/glstar/), the traces for L∗ in  [ife_results/lstar/](ife_results/lstar/), and a comparison in [ife_results/number_of_queries.csv](ife_results/number_of_queries.csv). For example, for n = 1, L* accepts the table [ife_results/lstar/final_table_0.015737.csv](ife_results/lstar/final_table_0.015737.csv) which induces the hypothesis [ife_results/lstar/final_hypothesis_0.015737.dot](ife_results/lstar/final_hypothesis_0.015737.dot); and GL* accepts the table [ife_results/glstar/final_table_0.01185.csv](ife_results/glstar/final_table_0.01185.csv) which induces the (embedded) hypothesis [ife_results/glstar/final_hypothesis_0.01185.dot](ife_results/glstar/final_hypothesis_0.01185.dot). Generally, we find that GL∗ outperforms L∗ for all choices of n. The difference in the number of membership queries increases with the size of n. For n = 9 the number of atoms is 2^9, resulting in an already relatively lare ge number of queries for both algorithms.

### Example 2

In [while_loop_results](while_loop_results) we present the results for the expression e =  (**while** t1 **do** p1); p2, the primitive actions {p1, p2}, and primitive tests {t1,...,tn} parametric in n = 1,...,9. The traces for GL∗ can be found in [while_loop_results/glstar/](while_loop_results/glstar/), the traces for L∗ in  [while_loop_results/lstar/](while_loop_results/lstar/), and a comparison in [while_loop_results/number_of_queries.csv](while_loop_results/number_of_queries.csv). Again, we find that GL∗ outperforms L∗ for all choices of n.

### More examples

It is possible to run the algorithms on arbitrary examples by modifying the following part in the file [test.ml](test.ml):
```
module Test2 = struct
  let e : ((int, int) Expressions.gkat_exp) = Comp(While (Do 1, Test 1), Do 2) in 
  (* let e : ((int, int) Expressions.gkat_exp) = Ife(Do 1, Test 1, Do 2) in  *)
  let max_number_of_tests = 9 in
  let number_of_actions = 2 in 
  (* let number_of_actions = 3 in  *)
  Test.test e max_number_of_tests number_of_actions;
end
```
The relevant syntax for defining expressions is defined in [expressions.ml](expressions.ml). Before running 
```
ocamlc -g -o test expressions.ml extra.ml automata.ml bisimulation.ml oracle.ml table.ml test.ml
```
make sure to clear the content of the folders [glstar](glstar) and [lstar](lstar).
