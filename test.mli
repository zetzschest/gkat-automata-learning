module Test :
  sig
    type word = Table2.Table2.word
    type ('a, 'b) coproduct = ('a, 'b) Automata.Automata.coproduct
    type one = Automata.Automata.one
    module Wordopt : sig type t = word option val compare : t -> t -> int end
    module Wordopt_One :
      sig
        type t = (word option, one) coproduct
        val compare : t -> t -> int
      end
    module O :
      sig
        module IntExp :
          sig
            type t = (int, int) Expressions.Expressions.kat_exp
            val compare : t -> t -> int
          end
        module O :
          sig
            module B :
              sig
                module Lexicographic :
                  functor (S1 : Set.OrderedType) (S2 : Set.OrderedType) ->
                    sig type t = S1.t * S2.t val compare : t -> t -> int end
                module R :
                  sig
                    type elt = Lexicographic(IntExp)(Wordopt).t
                    type t = Set.Make(Lexicographic(IntExp)(Wordopt)).t
                    val empty : t
                    val is_empty : t -> bool
                    val mem : elt -> t -> bool
                    val add : elt -> t -> t
                    val singleton : elt -> t
                    val remove : elt -> t -> t
                    val union : t -> t -> t
                    val inter : t -> t -> t
                    val disjoint : t -> t -> bool
                    val diff : t -> t -> t
                    val compare : t -> t -> int
                    val equal : t -> t -> bool
                    val subset : t -> t -> bool
                    val iter : (elt -> unit) -> t -> unit
                    val map : (elt -> elt) -> t -> t
                    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
                    val for_all : (elt -> bool) -> t -> bool
                    val exists : (elt -> bool) -> t -> bool
                    val filter : (elt -> bool) -> t -> t
                    val filter_map : (elt -> elt option) -> t -> t
                    val partition : (elt -> bool) -> t -> t * t
                    val cardinal : t -> int
                    val elements : t -> elt list
                    val min_elt : t -> elt
                    val min_elt_opt : t -> elt option
                    val max_elt : t -> elt
                    val max_elt_opt : t -> elt option
                    val choose : t -> elt
                    val choose_opt : t -> elt option
                    val split : elt -> t -> t * bool * t
                    val find : elt -> t -> elt
                    val find_opt : elt -> t -> elt option
                    val find_first : (elt -> bool) -> t -> elt
                    val find_first_opt : (elt -> bool) -> t -> elt option
                    val find_last : (elt -> bool) -> t -> elt
                    val find_last_opt : (elt -> bool) -> t -> elt option
                    val of_list : elt list -> t
                    val to_seq_from : elt -> t -> elt Seq.t
                    val to_seq : t -> elt Seq.t
                    val add_seq : elt Seq.t -> t -> t
                    val of_seq : elt Seq.t -> t
                  end
                val return_to_string :
                  'a Bisimulation.bisimul_return ->
                  ('a Bisimulation.word -> string) -> string
                val bisimulation :
                  ('x, 'a, 'b) Automata.Automata.moore_autom ->
                  ('y, 'a, 'b) Automata.Automata.moore_autom ->
                  ('x -> IntExp.t) ->
                  ('y -> Wordopt.t) ->
                  'a list ->
                  ('b -> 'b -> bool) -> 'a Bisimulation.bisimul_return
              end
            val oracle_from_autom :
              ('x, 'a, 'b) Automata.Automata.moore_autom ->
              ('x -> IntExp.t) ->
              ('y -> Wordopt.t) ->
              'a list ->
              ('b -> 'b -> bool) -> ('y, 'a, 'b) Oracle.Oracle.oracle
          end
        val oracle_from_kat_exp :
          (int, int) Expressions.Expressions.kat_exp ->
          int ->
          int ->
          ('y -> Wordopt.t) ->
          ('y, int list * int, int list -> bool) Oracle.Oracle.oracle
      end
    module OEmb :
      sig
        module IntExp :
          sig
            type t = (int, int) Expressions.Expressions.kat_exp
            val compare : t -> t -> int
          end
        module O :
          sig
            module B :
              sig
                module Lexicographic :
                  functor (S1 : Set.OrderedType) (S2 : Set.OrderedType) ->
                    sig type t = S1.t * S2.t val compare : t -> t -> int end
                module R :
                  sig
                    type elt = Lexicographic(IntExp)(Wordopt_One).t
                    type t = Set.Make(Lexicographic(IntExp)(Wordopt_One)).t
                    val empty : t
                    val is_empty : t -> bool
                    val mem : elt -> t -> bool
                    val add : elt -> t -> t
                    val singleton : elt -> t
                    val remove : elt -> t -> t
                    val union : t -> t -> t
                    val inter : t -> t -> t
                    val disjoint : t -> t -> bool
                    val diff : t -> t -> t
                    val compare : t -> t -> int
                    val equal : t -> t -> bool
                    val subset : t -> t -> bool
                    val iter : (elt -> unit) -> t -> unit
                    val map : (elt -> elt) -> t -> t
                    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
                    val for_all : (elt -> bool) -> t -> bool
                    val exists : (elt -> bool) -> t -> bool
                    val filter : (elt -> bool) -> t -> t
                    val filter_map : (elt -> elt option) -> t -> t
                    val partition : (elt -> bool) -> t -> t * t
                    val cardinal : t -> int
                    val elements : t -> elt list
                    val min_elt : t -> elt
                    val min_elt_opt : t -> elt option
                    val max_elt : t -> elt
                    val max_elt_opt : t -> elt option
                    val choose : t -> elt
                    val choose_opt : t -> elt option
                    val split : elt -> t -> t * bool * t
                    val find : elt -> t -> elt
                    val find_opt : elt -> t -> elt option
                    val find_first : (elt -> bool) -> t -> elt
                    val find_first_opt : (elt -> bool) -> t -> elt option
                    val find_last : (elt -> bool) -> t -> elt
                    val find_last_opt : (elt -> bool) -> t -> elt option
                    val of_list : elt list -> t
                    val to_seq_from : elt -> t -> elt Seq.t
                    val to_seq : t -> elt Seq.t
                    val add_seq : elt Seq.t -> t -> t
                    val of_seq : elt Seq.t -> t
                  end
                val return_to_string :
                  'a Bisimulation.bisimul_return ->
                  ('a Bisimulation.word -> string) -> string
                val bisimulation :
                  ('x, 'a, 'b) Automata.Automata.moore_autom ->
                  ('y, 'a, 'b) Automata.Automata.moore_autom ->
                  ('x -> IntExp.t) ->
                  ('y -> Wordopt_One.t) ->
                  'a list ->
                  ('b -> 'b -> bool) -> 'a Bisimulation.bisimul_return
              end
            val oracle_from_autom :
              ('x, 'a, 'b) Automata.Automata.moore_autom ->
              ('x -> IntExp.t) ->
              ('y -> Wordopt_One.t) ->
              'a list ->
              ('b -> 'b -> bool) -> ('y, 'a, 'b) Oracle.Oracle.oracle
          end
        val oracle_from_kat_exp :
          (int, int) Expressions.Expressions.kat_exp ->
          int ->
          int ->
          ('y -> Wordopt_One.t) ->
          ('y, int list * int, int list -> bool) Oracle.Oracle.oracle
      end
    val oracle_of_exp_gkat :
      (int, int) Expressions.Expressions.kat_exp ->
      int ->
      int ->
      ((word option, one) coproduct, int list * int, int list -> bool)
      Oracle.Oracle.oracle
    val oracle_of_exp :
      (int, int) Expressions.Expressions.kat_exp ->
      int ->
      int ->
      (word option, int list * int, int list -> bool) Oracle.Oracle.oracle
    val test :
      (int, int) Expressions.Expressions.gkat_exp -> int -> int -> unit
  end
module Test2 : sig end
