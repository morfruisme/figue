(* requis dans Make pour le type symbol *)
(* peut-être je devrais me restreindre à char uniquement *)
module type ToString = sig
  type t
  val to_string: t -> string
end

(* signature visible du module créé par Make *)
module type A = sig
  type sym
  type set

  (* aucune assomption sur le type output *)
  type 'o t = private {
    mutable n_states: int;
    mutable symbols: set;
    accept: (int, 'o) Hashtbl.t;
    delta: (int * sym, int) Hashtbl.t;
  }

  val empty: unit -> 'o t
  val new_state: 'o t -> int 
  val add_transition: 'o t -> int -> sym -> int -> bool
  val add_transition_fill: 'o t -> int -> sym -> int
  val is_accept: 'o t -> int -> bool
  val accept: 'o t -> int -> 'o -> unit
  val accept_word: 'o t -> sym list -> 'o -> unit 
  val follow: 'o t -> int -> sym -> int option
  val read: 'o t -> int -> 'o option
  val read_word: 'o t -> sym list -> 'o option

  val to_string: ?full: bool -> 'o t -> string
  val print: ?full: bool -> 'o t -> unit
end

module Make
  (Sym: sig
    type t
    include ToString with type t := t
    include Set.OrderedType with type t := t
  end)
  : A with type sym = Sym.t
  = struct

  module Set = Set.Make (Sym)

  type sym = Sym.t
  type set = Set.t

  type 'o t = {
    mutable n_states: int;
    mutable symbols: Set.t;
    accept: (int, 'o) Hashtbl.t;
    delta: (int * sym, int) Hashtbl.t;
  }



  (* basics *)

  let empty () = {
    n_states = 1;
    symbols = Set.empty;
    accept = Hashtbl.create 16;
    delta = Hashtbl.create 16;
  }

  let new_state a =
    let n = a.n_states in
    a.n_states <- n + 1;
    n

  let add_transition a q s q' =
    if Hashtbl.mem a.delta (q, s) then
      false
    else
      (Hashtbl.add a.delta (q, s) q';
      a.symbols <- Set.add s a.symbols;
      true) 
  
  let add_transition_fill a q s =
    match Hashtbl.find_opt a.delta (q, s) with
    | None ->
      let q' = new_state a in
      ignore @@ add_transition a q s q';
      q'
    | Some q' -> q'

   let is_accept a q =
    Hashtbl.mem a.accept q
    
  let accept a q o =
    Hashtbl.replace a.accept q o

  let accept_word a w o =
    let q = List.fold_left (add_transition_fill a) 0 w in
    accept a q o

  let follow a q s =
    Hashtbl.find_opt a.delta (q, s)

  let follow_word a q w =
    let rec f q w =
      match w with
      | [] -> Some q
      | s::w ->
        match follow a q s with
        | None -> None
        | Some q' -> f q' w in
    f q w

  let read a q =
    Hashtbl.find_opt a.accept q

  let read_word a w =
    match follow_word a 0 w with
    | None -> None
    | Some q -> read a q

  

  (* affichage *)

  let show_state ?(padding = 0) a q =
    let s = Misc.pad_to padding (string_of_int q) in
    if is_accept a q then
      Misc.bold s
    else
      s

  let show_transitions a q =
    let s =
      Set.fold
        (fun s acc ->
          match follow a q s with
          | None -> acc
          | Some q' ->
            Printf.sprintf "%s| %s → %s " acc (Sym.to_string s) (show_state a q'))
        a.symbols
        "" in
    if s = "" then
      ""
    else
      Printf.sprintf "%s %s\n" (show_state a q ~padding: 2) s

  let show_transitions_full ?(padding = 0) a q =
    Printf.sprintf "%s %s\n" (show_state a q ~padding: 2) @@
    Set.fold
      (fun s acc ->
        Printf.sprintf "%s|%s" acc @@
        match follow a q s with
        | None -> Misc.pad_to padding "-"
        | Some q' -> show_state a q' ~padding)
      a.symbols
      ""

  let to_string_full a =
    let padding = 1 + Misc.list_max (Set.to_list a.symbols) (fun x -> String.length (Sym.to_string x)) in

    let rec f q s =
      if q = a.n_states then
        s
      else 
        let s = s ^ show_transitions_full a q ~padding in
        f (q+1) s in

    let s =
      Printf.sprintf " \\ %s\n" @@
      Set.fold
        (fun s acc ->
          Printf.sprintf "%s|%s" acc @@
          Misc.pad_to padding @@
          (Sym.to_string s))
        a.symbols
        "" in

    f 0 s

  let to_string ?(full = false) a =
    if full then
      to_string_full a
    else
      let rec f q s =
        if q = a.n_states then
          s
        else
          let s = s ^ show_transitions a q in
          f (q+1) s in
      f 0 "Automate\n"

  let print ?(full = false) a =
    print_string (to_string a ~full)
end