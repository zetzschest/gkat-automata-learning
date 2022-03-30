module Table2 :
  sig
    type atom = int list
    type action = int
    type alphabet = (atom * action) list
    type word = (atom * action) list
    type word_plus_alpha = word * atom
    type bisimul_return = (atom * action) Bisimulation.bisimul_return
    module Words : sig type t = word val compare : t -> t -> int end
    module Words_plus_alpha :
      sig type t = word_plus_alpha val compare : t -> t -> int end
    module Atoms : sig type t = atom val compare : t -> t -> int end
    module Wordopt : sig type t = word option val compare : t -> t -> int end
    module Wordopt_One :
      sig
        type t =
            (word option, Automata.Automata.one) Automata.Automata.coproduct
        val compare : t -> t -> int
      end
    module P :
      sig
        module M :
          sig
            type key = Wordopt.t
            type 'a t = 'a Map.Make(Wordopt).t
            val empty : 'a t
            val is_empty : 'a t -> bool
            val mem : key -> 'a t -> bool
            val add : key -> 'a -> 'a t -> 'a t
            val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
            val singleton : key -> 'a -> 'a t
            val remove : key -> 'a t -> 'a t
            val merge :
              (key -> 'a option -> 'b option -> 'c option) ->
              'a t -> 'b t -> 'c t
            val union :
              (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
            val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
            val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
            val iter : (key -> 'a -> unit) -> 'a t -> unit
            val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
            val for_all : (key -> 'a -> bool) -> 'a t -> bool
            val exists : (key -> 'a -> bool) -> 'a t -> bool
            val filter : (key -> 'a -> bool) -> 'a t -> 'a t
            val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
            val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
            val cardinal : 'a t -> int
            val bindings : 'a t -> (key * 'a) list
            val min_binding : 'a t -> key * 'a
            val min_binding_opt : 'a t -> (key * 'a) option
            val max_binding : 'a t -> key * 'a
            val max_binding_opt : 'a t -> (key * 'a) option
            val choose : 'a t -> key * 'a
            val choose_opt : 'a t -> (key * 'a) option
            val split : key -> 'a t -> 'a t * 'a option * 'a t
            val find : key -> 'a t -> 'a
            val find_opt : key -> 'a t -> 'a option
            val find_first : (key -> bool) -> 'a t -> key * 'a
            val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
            val find_last : (key -> bool) -> 'a t -> key * 'a
            val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
            val map : ('a -> 'b) -> 'a t -> 'b t
            val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
            val to_seq : 'a t -> (key * 'a) Seq.t
            val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
            val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
            val of_seq : (key * 'a) Seq.t -> 'a t
          end
        val walk :
          ('x, 'a, 'b) Automata.Automata.moore_autom ->
          ('x -> Wordopt.t) -> 'a list -> ('a list M.t * 'b) M.t
        val autom_to_string :
          ('x, 'a, 'b) Automata.Automata.moore_autom ->
          ('x -> Wordopt.t) ->
          'a list ->
          ('a list -> string) ->
          ('b -> string) -> (Wordopt.t -> string) -> string
      end
    module PO :
      sig
        module M :
          sig
            type key = Wordopt_One.t
            type 'a t = 'a Map.Make(Wordopt_One).t
            val empty : 'a t
            val is_empty : 'a t -> bool
            val mem : key -> 'a t -> bool
            val add : key -> 'a -> 'a t -> 'a t
            val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
            val singleton : key -> 'a -> 'a t
            val remove : key -> 'a t -> 'a t
            val merge :
              (key -> 'a option -> 'b option -> 'c option) ->
              'a t -> 'b t -> 'c t
            val union :
              (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
            val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
            val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
            val iter : (key -> 'a -> unit) -> 'a t -> unit
            val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
            val for_all : (key -> 'a -> bool) -> 'a t -> bool
            val exists : (key -> 'a -> bool) -> 'a t -> bool
            val filter : (key -> 'a -> bool) -> 'a t -> 'a t
            val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
            val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
            val cardinal : 'a t -> int
            val bindings : 'a t -> (key * 'a) list
            val min_binding : 'a t -> key * 'a
            val min_binding_opt : 'a t -> (key * 'a) option
            val max_binding : 'a t -> key * 'a
            val max_binding_opt : 'a t -> (key * 'a) option
            val choose : 'a t -> key * 'a
            val choose_opt : 'a t -> (key * 'a) option
            val split : key -> 'a t -> 'a t * 'a option * 'a t
            val find : key -> 'a t -> 'a
            val find_opt : key -> 'a t -> 'a option
            val find_first : (key -> bool) -> 'a t -> key * 'a
            val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
            val find_last : (key -> bool) -> 'a t -> key * 'a
            val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
            val map : ('a -> 'b) -> 'a t -> 'b t
            val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
            val to_seq : 'a t -> (key * 'a) Seq.t
            val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
            val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
            val of_seq : (key * 'a) Seq.t -> 'a t
          end
        val walk :
          ('x, 'a, 'b) Automata.Automata.moore_autom ->
          ('x -> Wordopt_One.t) -> 'a list -> ('a list M.t * 'b) M.t
        val autom_to_string :
          ('x, 'a, 'b) Automata.Automata.moore_autom ->
          ('x -> Wordopt_One.t) ->
          'a list ->
          ('a list -> string) ->
          ('b -> string) -> (Wordopt_One.t -> string) -> string
      end
    module S :
      sig
        type elt = Words.t
        type t = Set.Make(Words).t
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
    module SA :
      sig
        type elt = Words_plus_alpha.t
        type t = Set.Make(Words_plus_alpha).t
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
    module M :
      sig
        type key = Words.t
        type 'a t = 'a Map.Make(Words).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
        val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
        val of_seq : (key * 'a) Seq.t -> 'a t
      end
    module MA :
      sig
        type key = Words_plus_alpha.t
        type 'a t = 'a Map.Make(Words_plus_alpha).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
        val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
        val of_seq : (key * 'a) Seq.t -> 'a t
      end
    type output = bool
    type ('a, 'b) coproduct = Left of 'a | Right of 'b
    type table = {
      top_rows : output MA.t M.t;
      bottom_rows : output MA.t M.t;
    }
    val top_row_indices : table -> S.t
    val bottom_row_indices : table -> S.t
    val column_indices : table -> SA.t
    val construct_new_row :
      word -> SA.t -> (word -> atom -> bool) -> output MA.t
    val word_times_alphabet : word -> alphabet -> word list
    type return_closed = None | Upper_Index of word
    val exists_equivalent_upper_row : output MA.t -> table -> return_closed
    val add_to_top_rows :
      word ->
      output MA.t ->
      table ->
      alphabet -> atom list -> (word -> atom -> bool) -> S.t -> SA.t -> table
    val word_to_string : word -> int -> string
    val output_to_string : output -> string
    val atom_to_string : atom -> int -> string
    val table_to_string : table -> int -> string
    type transition_structure = (bool, action * word) coproduct MA.t
    val is_nonzero : output MA.t -> SA.t -> bool
    type transition_structure_small = word M.t
    val close :
      table ->
      alphabet ->
      atom list ->
      (word -> atom -> bool) -> int -> int -> table * transition_structure
    val suffixes_of : word_plus_alpha -> word_plus_alpha list
    val add_counterexample :
      table ->
      word_plus_alpha -> (word -> atom -> bool) -> atom list -> table
    val instantiate_table :
      (word -> atom -> bool) -> atom list -> alphabet -> table
    val automaton_from_table_structure :
      transition_structure ->
      (word option, int, int) Automata.Automata.gkat_autom
    val extend_counterexample :
      word ->
      ((word option, Automata.Automata.one) Automata.Automata.coproduct, 
       int, int)
      Automata.Automata.kat_autom ->
      (word -> atom -> bool) -> atom list -> atom
    val glstar :
      (word -> atom -> bool) ->
      (((word option, Automata.Automata.one) Automata.Automata.coproduct,
        int, int)
       Automata.Automata.kat_autom -> bisimul_return) ->
      int ->
      int -> (word option, int, int) Automata.Automata.gkat_autom * int
  end
