module Automata :
  sig
    type ('x, 'a, 'b) moore_coalg = {
      delta : 'x -> 'a -> 'x;
      eps : 'x -> 'b;
    }
    type ('x, 'a, 'b) moore_autom = {
      initial : 'x;
      transition : ('x, 'a, 'b) moore_coalg;
    }
    type 't atom = 't list
    type ('x, 't, 'a) kat_coalg =
        ('x, 't atom * 'a, 't atom -> bool) moore_coalg
    type ('x, 't, 'a) kat_autom =
        ('x, 't atom * 'a, 't atom -> bool) moore_autom
    type ('a, 'b) coproduct = Left of 'a | Right of 'b
    type ('x, 't, 'a) gkat_coalg = {
      delta : 'x -> 't atom -> (bool, 'a * 'x) coproduct;
    }
    type ('x, 't, 'a) gkat_autom = {
      initial : 'x;
      transition : ('x, 't, 'a) gkat_coalg;
    }
    type 'a word = 'a list
    type one = One
    val embed :
      ('x, 't, 'a) gkat_autom -> (('x, one) coproduct, 't, 'a) kat_autom
    val reach_state : ('x, 'a, 'b) moore_autom -> 'a word -> 'x
    val reach : ('x, 'a, 'b) moore_autom -> 'a word -> 'b
    val deriv :
      ('t, 'a) Expressions.Expressions.kat_exp ->
      't list -> (('t, 'a) Expressions.Expressions.kat_exp, 't, 'a) kat_autom
    val autom_from_kat_exp :
      (int, int) Expressions.Expressions.kat_exp ->
      int -> ((int, int) Expressions.Expressions.kat_exp, int, int) kat_autom
  end
module Print :
  functor (Ord : Set.OrderedType) ->
    sig
      module M :
        sig
          type key = Ord.t
          type 'a t = 'a Map.Make(Ord).t
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
      val walk :
        ('x, 'a, 'b) Automata.moore_autom ->
        ('x -> Ord.t) -> 'a list -> ('a list M.t * 'b) M.t
      val autom_to_string :
        ('x, 'a, 'b) Automata.moore_autom ->
        ('x -> Ord.t) ->
        'a list ->
        ('a list -> string) -> ('b -> string) -> (Ord.t -> string) -> string
    end
