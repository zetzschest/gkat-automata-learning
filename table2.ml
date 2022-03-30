(* The GLstar algorithm for learning GKAT automata. *)
module Table2 = struct
  open Automata
  open Extra

  type atom = int list
  type action = int 
  type alphabet = (atom * action) list
  type word = (atom * action) list 
  type word_plus_alpha = word * atom
  type bisimul_return = (atom * action) Bisimulation.bisimul_return

  module Words : Set.OrderedType with type t = word = struct
    type t = word
    let compare xs ys = 
      Stdlib.compare xs ys
  end  

  module Words_plus_alpha : Set.OrderedType with type t = word_plus_alpha = struct
    type t = word_plus_alpha
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

  module Wordopt_One : Set.OrderedType with type t = (word option, Automata.one) Automata.coproduct  = struct
    type t = (word option, Automata.one) Automata.coproduct 
    
    let compare x y = 
        Stdlib.compare x y
  end

  module P = Print(Wordopt)
  module PO = Print(Wordopt_One)
  module S = Set.Make(Words)
  module SA = Set.Make(Words_plus_alpha)
  module M = Map.Make(Words)
  module MA = Map.Make(Words_plus_alpha)

  type output = bool

  type ('a, 'b) coproduct = Left of 'a | Right of 'b

  (* The table here differs to the one used in LStar in the typs of the columns and entries. *)
  type table = {
    top_rows: (output MA.t) M.t; 
    bottom_rows: (output MA.t) M.t; 
  }

  (* Derives the top row, bottom row, and column indices of the table. *)
  let top_row_indices (t: table) : S.t =
    S.of_list (List.map (fun x -> fst x) (M.bindings t.top_rows)) 

  let bottom_row_indices (t: table) : S.t =
    S.of_list (List.map (fun x -> fst x) (M.bindings t.bottom_rows)) 

  let column_indices (t: table) : SA.t = 
    SA.of_list (List.map (fun x -> fst x) (MA.bindings (snd (M.min_binding t.top_rows))))

  (* Constructs a new row indexed by a given word. *)
  let construct_new_row (w: word) (column_indices_set: SA.t) (mq: word -> atom -> bool) : output MA.t =
    let column_indices_list = SA.elements column_indices_set in 
    let rec f (w_row: output MA.t) (todo_columns: word_plus_alpha list) = 
      match todo_columns with 
      | [] -> w_row
      | p :: tl -> 
        let e = fst p in 
        let alpha = snd p in 
        let entry = mq (w @ e) alpha in  
        let new_w_row = MA.add p entry w_row in 
        f new_w_row tl
    in f MA.empty column_indices_list

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
  let exists_equivalent_upper_row (row: output MA.t) (t: table) : return_closed =
    let f (v: word) : bool =
      let row_at_v : output MA.t = M.find v t.top_rows in 
      MA.equal Bool.equal row_at_v row
    in 
    let equivalent_top_rows = S.filter f (top_row_indices t) in
      match S.is_empty equivalent_top_rows with 
      | true -> None
      | false -> Upper_Index (S.min_elt equivalent_top_rows)
 
  (* Adds a given row to the upper part of a table, and extends the lower part as necessary. *)
  let add_to_top_rows (w: word) (row: output MA.t) (t: table) (a: alphabet) (atoms: atom list) (mq: word -> atom -> bool) (current_bottom_row_indices: S.t) (column_indices_set: SA.t) : table =
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
    let rec g (todo: word list) (m: (output MA.t) M.t) : (output MA.t) M.t =
      match todo with 
      | [] -> m
      | hd :: tl -> 
        let row_at_hd = construct_new_row hd column_indices_set mq in 
        g tl (M.add hd row_at_hd m )
    in
    let new_bottom_rows = g bottom_row_indices_to_be_added t.bottom_rows in 
    { top_rows = new_top_rows; bottom_rows = new_bottom_rows; }

  (* Turns a word into a string. *)  
  let word_to_string (w: word) (number_of_tests: int) : string =
    let tests = Extra.range_from_one(number_of_tests) in
    Extra.string_atoms_and_actions w tests

  (* Turns an element of output type (bool) into a string. *)
  let output_to_string (o: output) : string =
    match o with 
    | false -> "0"
    | true -> "1"

  (* Turns an atom to a string. *)
  let atom_to_string (alpha: atom) (number_of_tests: int) : string =
    let tests = Extra.range_from_one(number_of_tests) in
    Extra.atom_to_string alpha tests 

  (* Turns a table into a string. *)
  let table_to_string (t: table) (number_of_tests: int) : string =
    let top_row_indices_list = S.elements (top_row_indices t) in 
    let bottom_row_indices_list = S.elements (bottom_row_indices t) in 
    let column_indices_list = SA.elements (column_indices t) in 
    let rec row_to_string (todo: word_plus_alpha list) (r: output MA.t) (str: string) : string =
      match todo with 
      | [] -> str 
      | hd :: tl -> 
        let entry = MA.find hd r in 
        row_to_string tl r (str ^ (output_to_string entry) ^ "," )
    in  
    let rec rows_to_string (todo: word list) (rows: (output MA.t) M.t) (str: string) : string =
      match todo with 
      | [] -> str
      | hd :: tl -> 
        let row_at_hd = M.find hd rows in 
        let row_at_hd_string = (word_to_string hd number_of_tests) ^ "," ^ (row_to_string column_indices_list row_at_hd "") in 
        rows_to_string tl rows (str ^ row_at_hd_string ^ "\n")
    in
    let rec column_indices_to_string (todo: word_plus_alpha list) (str: string) : string =
      match todo with 
      | [] -> str
      | hd :: tl -> 
        let e = fst hd in 
        let alpha = snd hd in
        column_indices_to_string tl (str ^ (word_to_string e number_of_tests) ^ (atom_to_string alpha number_of_tests) ^ ",")
    in 
    let column_indices_string = (column_indices_to_string column_indices_list ",") ^ "\n" in 
    let top_rows_string = rows_to_string top_row_indices_list t.top_rows "" in 
    let bottom_rows_string = rows_to_string bottom_row_indices_list t.bottom_rows "" in
    column_indices_string ^ top_rows_string ^ "\n" ^ bottom_rows_string
  
  type transition_structure = ((bool, action * word) coproduct) MA.t 

  (* Checks whether a row contains a non-zero entry. *)
  let is_nonzero (r: output MA.t) (column_indices_set: SA.t) : bool = 
    let column_indices_list = SA.elements column_indices_set in 
    let rec f (todo: word_plus_alpha list) : bool =
      match todo with 
      | [] -> false
      | hd :: tl -> 
        if (MA.find hd r) then
          true
        else
          f tl 
    in f column_indices_list 

  type transition_structure_small = word M.t  
  
  (* Closes a table, and derives a "transition structure", which will be used for the automaton that the (closed) table induces. *)
  let rec close (t: table) (a: alphabet) (atoms: atom list) (mq: word -> atom -> bool) (number_of_tests: int) (number_of_actions: int)  : table * transition_structure =
    let bottom_row_indices_list = S.elements (bottom_row_indices t) in 
    let rec first_close (t': table) (todo: word list) (trans_struc_small: transition_structure_small) : table * transition_structure_small =
      match todo with 
      | [] -> (t', trans_struc_small)
      | w :: tl -> 
        let transition_index = w in 
        let transition_row = M.find transition_index t'.bottom_rows in
        let column_indices_set = column_indices t' in  
        let transition_row_is_nonzero = is_nonzero transition_row column_indices_set in 
        if transition_row_is_nonzero then 
          match exists_equivalent_upper_row transition_row t' with 
          | None -> 
            let table_without_hd = { top_rows = t'.top_rows; bottom_rows = (M.remove transition_index t'.bottom_rows); } in 
            let new_table = add_to_top_rows transition_index transition_row table_without_hd a atoms mq (bottom_row_indices table_without_hd) (column_indices table_without_hd) in 
            first_close new_table (S.elements (bottom_row_indices table_without_hd)) M.empty
          | Upper_Index s -> 
            let new_trans_struc_small = M.add w s trans_struc_small in 
            first_close t' tl new_trans_struc_small
        else 
          first_close t' tl trans_struc_small
    in 
    let tup = first_close t bottom_row_indices_list M.empty in 
    let closed_table = fst tup in 
    let trans_struc_small = snd tup in  
    let actions = Extra.range_from_one(number_of_actions) in 
    let top_row_indices_list = S.elements (top_row_indices closed_table) in 
    let rec f (todo: word list) (trans_struc: transition_structure) : table * transition_structure =
      match todo with 
      | [] -> (closed_table, trans_struc)
      | hd :: tl -> 
        let rec g (todo': atom list) (trans_struc': transition_structure) : table * transition_structure =
          match todo' with 
          | [] -> f tl trans_struc'
          | alpha :: tl' ->
            if mq hd alpha then 
              let new_trans_struc = MA.add (hd, alpha) (Left true) trans_struc' in 
              g tl' new_trans_struc
            else
              let rec h (todo'': action list) (trans_struc'': transition_structure) : table * transition_structure =
                match todo'' with 
                | [] -> 
                  let new_trans_struc = MA.add (hd, alpha) (Left false) trans_struc'' in 
                  g tl' new_trans_struc
                | p :: tl'' ->
                  let transition_index = (hd @ [(alpha, p)]) in
                  if M.mem transition_index closed_table.top_rows then 
                    let new_trans_struc = MA.add (hd, alpha) (Right (p, transition_index)) trans_struc'' in 
                    g tl' new_trans_struc
                  else 
                    let transition_row = M.find transition_index closed_table.bottom_rows in 
                    let transition_row_is_nonzero = is_nonzero transition_row (column_indices closed_table) in 
                    if transition_row_is_nonzero then 
                      let new_trans_struc = MA.add (hd, alpha) (Right (p, (M.find transition_index trans_struc_small))) trans_struc'' in 
                      g tl' new_trans_struc
                    else
                      h tl'' trans_struc''
              in h actions trans_struc'   
        in g atoms trans_struc
    in f top_row_indices_list MA.empty
    
  (* Constructs all the suffixes of a given word. Note that in contrast to Lstar, a word ends in an additional atom. *)
  let suffixes_of (w: word_plus_alpha) : (word_plus_alpha list) =
    let rec f (todo: word_plus_alpha) (xs: word_plus_alpha list) =
      let todo_word = fst todo in 
      let alpha = snd todo in 
      match todo_word with 
      | [] -> todo :: xs
      | _ :: tl -> f (tl,alpha) (todo :: xs)
    in f w []

  (* 
  Extends a table with a counterexample by adding its suffixes to the column indices. Note that in contrast to Lstar, a
  counterexample ends in an additional atom. 
  *)  
  let add_counterexample (t: table) (z: word_plus_alpha) (mq: word -> atom -> bool) (atoms: atom list) : table = 
    let suffixes_of_z = suffixes_of z in 
    let column_indices_list = SA.elements (column_indices t) in
    let relevant_suffixes_of_z = List.filter (fun v -> not (List.mem v column_indices_list)) suffixes_of_z in
    let top_row_indices_list = S.elements (top_row_indices t) in 
    let bottom_row_indices_list = S.elements (bottom_row_indices t) in 
    let rec update_row (w: word) (todo_suffixes: word_plus_alpha list) (current_row_at_w: output MA.t) : output MA.t =
      match todo_suffixes with 
      | [] -> current_row_at_w
      | p :: tl -> 
        let e = fst p in 
        let alpha = snd p in 
        let entry_at_wp = mq (w @ e) alpha in 
        let new_row_at_w = MA.add p entry_at_wp current_row_at_w in 
        update_row w tl new_row_at_w
    in 
    let rec update_rows (todo_indices: word list) (current_rows: (output MA.t) M.t) : (output MA.t) M.t =
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

  (* Instantiates a table. In contrast to Lstar, columns are not indexed by the empty word, but by atoms. *)
  let instantiate_table (mq: word -> atom -> bool) (atoms: atom list) (a: alphabet) : table =
    let rec f (todo: atom list) (s: SA.t) : SA.t = 
      match todo with 
      | [] -> s  
      | alpha :: tl -> f tl (SA.add ([], alpha) s) 
    in 
    let set_of_atoms = f atoms SA.empty in 
    let rec g (todo: atom list) (r: output MA.t) : output MA.t = 
      match todo with 
      | [] -> r 
      | alpha :: tl -> g tl (MA.add ([], alpha) (mq [] alpha) r)
    in 
    let row_at_empty_word = g atoms MA.empty in
    let empty_table = { top_rows = M.empty; bottom_rows = M.empty; } in 
    add_to_top_rows [] row_at_empty_word empty_table a atoms mq (S.empty) set_of_atoms

  (* Derives a GKAT automaton from a (closed) table. *)
  let automaton_from_table_structure (trans: transition_structure) : (word option, int, int) Automata.gkat_autom =
    let delta' (wo: word option) (alpha: atom) : (bool, int * (word option)) Automata.coproduct  =
      match wo with 
      | None -> Left false
      | Some w -> 
        match MA.find_opt (w, alpha) trans with 
        | None -> Left false
        | Some (Left b) -> Left b
        | Some (Right tup) -> 
          let p = fst tup in 
          let v = snd tup in
          Right (p, Some v)
    in 
    { initial = Some []; transition = { delta = delta'; }; }

  (* Calculates the additional atom witnessing an (extended) counterexample. *)
  let extend_counterexample (w: word) (autom: ((word option, Automata.one) Automata.coproduct, int, int) Automata.kat_autom) (mq: word -> atom -> bool) (atoms: atom list) : atom =
    let state_by_w = Automata.reach_state autom w in 
    let output_by_w = autom.transition.eps state_by_w in 
    let mq_by_w = mq w in 
    let rec f (todo: atom list) : atom =
      match todo with 
      | [] -> []
      | alpha :: tl -> 
        if output_by_w alpha = mq_by_w alpha then
          f tl 
        else
          alpha 
    in f atoms  
 
  (* Learns a GKAT automaton via the GLstar algorithm. Also outputs the number of membership queries required. *) 
  let glstar (mq: word -> atom -> bool) (eq: ((word option, Automata.one) Automata.coproduct, int, int) Automata.kat_autom -> bisimul_return) (number_of_tests: int) (number_of_actions: int) : (word option, int, int) Automata.gkat_autom * int =
    let tests = Extra.range_from_one(number_of_tests) in
    let actions = Extra.range_from_one(number_of_actions) in 
    let a = Extra.from_tests_and_actions_to_atoms_and_actions tests actions in
    let atoms = Extra.from_tests_to_atoms tests in 
    let initial_table = instantiate_table mq atoms a in
    let rec f (current_table: table) : (word option, int, int) Automata.gkat_autom * int =
      let tuple = close current_table a atoms mq number_of_tests number_of_actions in 
      let closed_table = fst tuple in
      let trans_struc = snd tuple in
      let hypothesis_gkat_automaton = automaton_from_table_structure trans_struc in 
      let hypothesis_kat_automaton = Automata.embed hypothesis_gkat_automaton in 
      let table_string = table_to_string closed_table number_of_tests in 
      let time = string_of_float (Sys.time ()) in 
      let to_string_a (xs: ((int list) * int) list) : string = 
        Extra.string_atoms_and_actions xs tests
      in 
      let to_string_b (f: (int list) -> bool) : string = 
        let g alpha = 
            Extra.atom_to_string alpha tests
        in Extra.subsets_of_atoms_to_string f g atoms
      in 
      let to_string_x (wo_one: (word option, Automata.one) Automata.coproduct) : string =
        match wo_one with
        | Left wo ->
            (match wo with 
            | None -> "false"
            | Some w -> to_string_a w)
        | Right _ -> "sinkstate"
      in
      let autom_string = PO.autom_to_string hypothesis_kat_automaton (fun x -> x) a to_string_a to_string_b to_string_x in 
      match eq hypothesis_kat_automaton with 
      | True -> 
        let oc = open_out ("glstar/final_hypothesis_"^time^".dot") in
        Printf.fprintf oc "%s" autom_string;
        close_out oc;
        let oc = open_out ("glstar/final_table_"^time^".csv") in
        Printf.fprintf oc "%s" table_string;
        close_out oc;
        let number_of_rows = (M.cardinal closed_table.top_rows) + (M.cardinal closed_table.bottom_rows) in
        let number_of_columns = MA.cardinal (snd (M.min_binding closed_table.top_rows)) in 
        let number_of_mq = number_of_rows * number_of_columns in 
        (hypothesis_gkat_automaton, number_of_mq)
      | Counterexample w -> 
        let oc = open_out ("glstar/hypothesis_"^time^".dot") in
        Printf.fprintf oc "%s" autom_string;
        close_out oc;
        let oc = open_out ("glstar/table_"^time^".csv") in
        Printf.fprintf oc "%s" table_string;
        close_out oc;
        let alpha = extend_counterexample w hypothesis_kat_automaton mq atoms in 
        let table_with_counterexample = add_counterexample closed_table (w, alpha) mq atoms in
        f table_with_counterexample
    in f initial_table
    

end
