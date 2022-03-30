(* The Lstar algorithm for learning KAT-automata. *)

module Table = struct
  open Automata
  open Extra

  type atom = int list
  type action = int 
  type alphabet = (atom * action) list
  type word = (atom * action) list 
  type bisimul_return = (atom * action) Bisimulation.bisimul_return

  module Words : Set.OrderedType with type t = word = struct
    type t = word
    let compare xs ys = 
      Stdlib.compare xs ys
  end  
  
  module Atoms : Set.OrderedType with type t = atom = struct
    type t = atom
    let compare xs ys = 
      Stdlib.compare xs ys
  end 

  module Wordopt : Set.OrderedType with type t = word option = struct
    type t = word option 
    
    let compare x y = 
      Stdlib.compare x y
  end

  module P = Print(Wordopt)
  module S = Set.Make(Words)
  module SA = Set.Make(Atoms)
  module M = Map.Make(Words)

  type output = SA.t

  (* Defines a table in the standard way. In the present KAT case, the entries are atoms. *)
  type table = {
    top_rows: (output M.t) M.t; 
    bottom_rows: (output M.t) M.t; 
  }

  (* Derives the top row, bottom row, and column indices of the table. *)
  let top_row_indices (t: table) : S.t =
    S.of_list (List.map (fun x -> fst x) (M.bindings t.top_rows)) 

  let bottom_row_indices (t: table) : S.t =
    S.of_list (List.map (fun x -> fst x) (M.bindings t.bottom_rows)) 

  let column_indices (t: table) : S.t = 
    S.of_list (List.map (fun x -> fst x) (M.bindings (snd (M.min_binding t.top_rows))))

  (* Constructs a new row indexed by a given word. *)
  let construct_new_row (w: word) (column_indices_set: S.t) (atoms: atom list) (mq: word -> atom -> bool) : output M.t =
    let column_indices_list = S.elements column_indices_set in 
    let rec f (w_row: output M.t) (todo_columns: word list) = 
      match todo_columns with 
      | [] -> w_row
      | e :: tl -> 
        let rec construct_entry (todo: atom list) (current_set: SA.t) : SA.t =
          match todo with 
          | [] -> current_set
          | alpha :: tl' ->
            if mq (w @ e) alpha then
              let new_current_set = SA.add alpha current_set in 
              construct_entry tl' new_current_set
            else
              construct_entry tl' current_set
        in  
        let entry = construct_entry atoms SA.empty in 
        let new_w_row = M.add e entry w_row in 
        f new_w_row tl
    in f M.empty column_indices_list

  (* For a given word "w", constructs all possible continuations "wc", for "c" a character in the alphabet "a". *)
  let word_times_alphabet (w: word) (a: alphabet) : word list =
    let rec g (todo: alphabet) (xs: word list) : word list =
      match todo with
      | [] -> xs
      | c :: tl -> 
        let word_times_char : word = w @ [c] in 
        g tl (word_times_char :: xs)
    in g a []

  type return_closed = None | Upper_Index of word
    
  (* Checks whether a row appears in the upper part of a table. *)
  let exists_equivalent_upper_row (row: output M.t) (t: table) : return_closed =
    let f (v: word) : bool =
      let row_at_v : output M.t = M.find v t.top_rows in 
      M.equal SA.equal row_at_v row
    in 
    let equivalent_top_rows = S.filter f (top_row_indices t) in
      match S.is_empty equivalent_top_rows with 
      | true -> None
      | false -> Upper_Index (S.min_elt equivalent_top_rows)

  (* Adds a given row to the upper part of a table, and extends the lower part as necessary. *)
  let add_to_top_rows (w: word) (row: output M.t) (t: table) (a: alphabet) (atoms: atom list) (mq: word -> atom -> bool) (current_bottom_row_indices: S.t) (column_indices_set: S.t) : table =
    let new_top_rows = M.add w row t.top_rows in 
    let transition_indices = word_times_alphabet w a in 
    let rec f (todo: word list) (xs : word list) : word list =  
      match todo with 
      | [] -> xs
      | hd :: tl -> 
        if S.mem hd current_bottom_row_indices then
          f tl xs
        else
          f tl (hd :: xs)
    in
    let bottom_row_indices_to_be_added = f transition_indices [] in 
    let rec g (todo: word list) (m: (output M.t) M.t) : (output M.t) M.t =
      match todo with 
      | [] -> m 
      | hd :: tl -> 
        let row_at_hd = construct_new_row hd column_indices_set atoms mq in 
        g tl (M.add hd row_at_hd m )
    in
    let new_bottom_rows = g bottom_row_indices_to_be_added t.bottom_rows in 
    { top_rows = new_top_rows; bottom_rows = new_bottom_rows; }

  (* Turns a word into a string. *)  
  let word_to_string (w: word) (number_of_tests: int) : string =
    let tests = Extra.range_from_one(number_of_tests) in
    Extra.string_atoms_and_actions w tests

  (* Turns an element of output type into a string. *)
  let output_to_string (o: output) (number_of_tests: int) : string =
    let output_list = SA.elements o in 
    let tests = Extra.range_from_one(number_of_tests) in
    "{" ^ ((Extra.string_list (fun alpha -> (Extra.atom_to_string alpha tests) ^ ";")) output_list) ^ "}"

  (* Turns a table into a string. *)
  let table_to_string (t: table) (number_of_tests: int) : string =
    let top_row_indices_list = S.elements (top_row_indices t) in 
    let bottom_row_indices_list = S.elements (bottom_row_indices t) in 
    let column_indices_list = S.elements (column_indices t) in 
    let rec row_to_string (todo: word list) (r: output M.t) (str: string) : string =
      match todo with 
      | [] -> str 
      | hd :: tl -> 
        let entry = M.find hd r in 
        row_to_string tl r (str ^ (output_to_string entry number_of_tests) ^ "," )
    in  
    let rec rows_to_string (todo: word list) (rows: (output M.t) M.t) (str: string) : string =
      match todo with 
      | [] -> str
      | hd :: tl -> 
        let row_at_hd = M.find hd rows in 
        let row_at_hd_string = (word_to_string hd number_of_tests) ^ "," ^ (row_to_string column_indices_list row_at_hd "") in 
        rows_to_string tl rows (str ^ row_at_hd_string ^ "\n")
    in
    let rec column_indices_to_string (todo: word list) (str: string) : string =
      match todo with 
      | [] -> str
      | hd :: tl -> column_indices_to_string tl (str ^ (word_to_string hd number_of_tests) ^ ",")
    in 
    let column_indices_string = (column_indices_to_string column_indices_list ",") ^ "\n" in 
    let top_rows_string = rows_to_string top_row_indices_list t.top_rows "" in 
    let bottom_rows_string = rows_to_string bottom_row_indices_list t.bottom_rows "" in
    column_indices_string ^ top_rows_string ^ "\n" ^ bottom_rows_string
  
  type transition_structure = word M.t 

  (* Closes a table, and derives a "transition structure", which will be used for the automaton that the (closed) table induces. *)
  let rec close (t: table) (a: alphabet) (atoms: atom list) (mq: word -> atom -> bool) (number_of_tests: int) : table * transition_structure =
    let bottom_row_indices_list = S.elements (bottom_row_indices t) in 
    let top_row_indices_list = S.elements (top_row_indices t) in 
    let rec g (todo: word list) (trans_struc: transition_structure) : transition_structure =
      match todo with 
      | [] -> trans_struc 
      | hd :: tl -> g tl (M.add hd hd trans_struc)
    in 
    let initial_trans_struc = g top_row_indices_list M.empty in 
    let rec f (todo: word list) (trans_struc: transition_structure) : table * transition_structure = 
      match todo with 
      | [] -> (t, trans_struc)
      | hd :: tl -> 
        let transition_row = M.find hd t.bottom_rows in 
        match exists_equivalent_upper_row transition_row t with 
        | None -> 
          let table_without_hd = { top_rows = t.top_rows; bottom_rows = (M.remove hd t.bottom_rows); } in 
          let new_table = add_to_top_rows hd transition_row table_without_hd a atoms mq (bottom_row_indices table_without_hd) (column_indices table_without_hd) in 
          close new_table a atoms mq number_of_tests
        | Upper_Index s -> 
          let new_trans_struc = M.add hd s trans_struc in 
          f tl new_trans_struc
    in f bottom_row_indices_list initial_trans_struc 

  type output_structure = SA.t M.t

  (* Assigns to a table an "output structure", which will be used for the automaton that the (closed) table induces. *)
  let construct_output_structure (t: table) : output_structure =
    let top_row_indices_list = S.elements (top_row_indices t) in 
    let rec f (todo: word list) (current_output_structure: output_structure) : output_structure =
      match todo with 
      | [] -> current_output_structure
      | hd :: tl -> 
        let hd_row = M.find hd t.top_rows in 
        let eps_at_hd_row = M.find [] hd_row in 
        let new_output_structure = M.add hd eps_at_hd_row current_output_structure in 
        f tl new_output_structure
    in f top_row_indices_list M.empty

  (* Constructs all the suffixes of a given word. *)
  let suffixes_of (w: word) : (word list) =
    let rec f (todo: word) (xs: word list) =
      match todo with 
      | [] -> xs
      | hd :: tl -> f tl (todo :: xs)
    in f w []

  (* Extends a table with a counterexample by adding its suffixes to the column indices. *)  
  let add_counterexample (t: table) (z: word) (mq: word -> atom -> bool) (atoms: atom list) : table = 
    let suffixes_of_z = suffixes_of z in 
    let column_indices_list = S.elements (column_indices t) in
    let relevant_suffixes_of_z = List.filter (fun v -> not (List.mem v column_indices_list)) suffixes_of_z in
    let top_row_indices_list = S.elements (top_row_indices t) in 
    let bottom_row_indices_list = S.elements (bottom_row_indices t) in 
    let rec update_row (w: word) (todo_suffixes: word list) (current_row_at_w: output M.t) : output M.t =
      match todo_suffixes with 
      | [] -> current_row_at_w
      | e :: tl -> 
        let rec construct_entry (todo_atoms: atom list) (current_set: SA.t) : SA.t =
          match todo_atoms with 
          | [] -> current_set
          | alpha :: tl' ->
            if mq (w @ e) alpha then
              let new_current_set = SA.add alpha current_set in 
              construct_entry tl' new_current_set
            else
              construct_entry tl' current_set
        in 
        let entry_at_we = construct_entry atoms SA.empty in 
        let new_row_at_w = M.add e entry_at_we current_row_at_w in 
        update_row w tl new_row_at_w
    in 
    let rec update_rows (todo_indices: word list) (current_rows: (output M.t) M.t) : (output M.t) M.t =
      match todo_indices with 
      | [] -> current_rows
      | w :: tl -> 
        let row_at_w = M.find w current_rows in 
        let new_row_at_w = update_row w relevant_suffixes_of_z row_at_w in 
        let rows_without_w = M.remove w current_rows in
        let rows_with_new_w = M.add w new_row_at_w rows_without_w in 
        update_rows tl rows_with_new_w 
    in
    let new_top_rows = update_rows top_row_indices_list t.top_rows in 
    let new_bottom_rows = update_rows bottom_row_indices_list t.bottom_rows in 
    { top_rows = new_top_rows; bottom_rows = new_bottom_rows; }

  (* Instantiates a table using the empty word. *)
  let instantiate_table (mq: word -> atom -> bool) (atoms: atom list) (a: alphabet) : table =
    let accepted_atoms = SA.of_list (List.filter (fun alpha -> mq [] alpha) atoms) in
    let row_at_empty_word = M.add [] accepted_atoms M.empty in
    let empty_table = { top_rows = M.empty; bottom_rows = M.empty; } in 
    add_to_top_rows [] row_at_empty_word empty_table a atoms mq (S.empty) (S.add [] S.empty)

  (* Derives a KAT automaton from a (closed) table. *)
  let automaton_from_table_structure (trans: transition_structure) (out: output_structure) : (word option, int, int) Automata.kat_autom =
    let delta' (wo: word option) (inp: atom * action) : word option =
      match wo with 
      | None -> None
      | Some w -> M.find_opt (w @ [inp]) trans
    in 
    let eps' (wo: word option) (alpha: atom) : bool =
      match wo with 
      | None -> false
      | Some w -> SA.mem alpha (M.find w out)
    in 
    { initial = Some []; transition = { delta = delta'; eps = eps'; }; }

  (* Learns a KAT automaton via the Lstar algorithm. Also outputs the number of membership queries required. *) 
  let lstar_moore (mq: word -> atom -> bool) (eq: (word option, int, int) Automata.kat_autom -> bisimul_return) (number_of_tests: int) (number_of_actions: int) : (word option, int, int) Automata.kat_autom * int =
    let tests = Extra.range_from_one(number_of_tests) in
    let actions = Extra.range_from_one(number_of_actions) in 
    let a = Extra.from_tests_and_actions_to_atoms_and_actions tests actions in
    let atoms = Extra.from_tests_to_atoms tests in 
    let initial_table = instantiate_table mq atoms a in
    let to_string_a (w: word) : string = 
      word_to_string w number_of_tests
    in
    let to_string_b (f: (int list) -> bool) : string = 
      let g alpha = 
          Extra.atom_to_string alpha tests
      in Extra.subsets_of_atoms_to_string f g atoms
    in 
    let to_string_x (wo: word option) : string =
      match wo with
      | None -> "false"
      | Some w -> to_string_a w
    in 
    let rec f (current_table: table) : (word option, int, int) Automata.kat_autom * int =
      let tuple = close current_table a atoms mq number_of_tests in 
      let closed_table = fst tuple in
      let trans_struc = snd tuple in
      let output_struc = construct_output_structure closed_table in 
      let hypothesis_automaton = automaton_from_table_structure trans_struc output_struc in 
      let table_string = table_to_string closed_table number_of_tests in 
      let autom_string = P.autom_to_string hypothesis_automaton (fun x -> x) a to_string_a to_string_b to_string_x in 
      let time = string_of_float (Sys.time ()) in 
      match eq hypothesis_automaton with 
      | True -> 
        let oc = open_out ("lstar/final_hypothesis_"^time^".dot") in
        Printf.fprintf oc "%s" autom_string;
        close_out oc;
        let oc = open_out ("lstar/final_table_"^time^".csv") in
        Printf.fprintf oc "%s" table_string;
        close_out oc;
        let number_of_rows = (M.cardinal closed_table.top_rows) + (M.cardinal closed_table.bottom_rows) in
        let number_of_columns = M.cardinal (snd (M.min_binding closed_table.top_rows)) in 
        let number_of_atoms = List.length atoms in 
        let number_of_mq = number_of_rows * number_of_columns * number_of_atoms in 
        (hypothesis_automaton, number_of_mq)
      | Counterexample z -> 
        let oc = open_out ("lstar/hypothesis_"^time^".dot") in
        Printf.fprintf oc "%s" autom_string;
        close_out oc;
        let oc = open_out ("lstar/table_"^time^".csv") in
        Printf.fprintf oc "%s" table_string;
        close_out oc;
        let table_with_counterexample = add_counterexample closed_table z mq atoms in
        f table_with_counterexample
    in f initial_table  
end