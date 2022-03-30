module Extra :
  sig
    type 't atom = 't list
    val atom_to_string : int atom -> int list -> string
    val string_list : ('a -> string) -> 'a list -> string
    val string_two_lists :
      ('a -> string) -> ('b -> string) -> ('a * 'b) list -> string
    val string_tests : int -> string
    val string_actions : int -> string
    val string_atoms_and_actions :
      (int list * int) list -> int list -> string
    val from_tests_to_atoms : 't list -> 't atom list
    val list_times_list : 'a list -> 'b list -> ('a * 'b) list
    val from_tests_and_actions_to_atoms_and_actions :
      't list -> 'a list -> ('t atom * 'a) list
    val subsets_of_atoms_to_string :
      ('a atom -> bool) -> ('a atom -> string) -> 'a atom list -> string
    val range_from_one : int -> int list
  end
