module type ToString = sig
  type t
  val to_string: t -> string
end

module type A = sig
  type i
  type set

  type 'o t = private {
    mutable n_states: int;
    mutable symbols: set;
    accept: (int, 'o) Hashtbl.t;
    delta: (int * i, int) Hashtbl.t;
  }

  val empty: unit -> 'o t
  val new_state: 'o t -> int
  val set_accept: 'o t -> int -> 'o -> unit
  val is_accept: 'o t -> int -> bool
  val add_transition: 'o t -> int -> i -> int -> bool
  val add_transition_fill: 'o t -> int -> i -> int option
  val read: 'o t -> int -> i -> int option
  val to_string: 'o t -> string
  val print: 'o t -> unit
end

module Make
  (Input: sig
    type t
    include ToString with type t := t
    include Set.OrderedType with type t := t
  end)
  : A with type i = Input.t
  = struct

  module Set = Set.Make (Input)

  type i = Input.t
  type set = Set.t

  type 'o t = {
    mutable n_states: int;
    mutable symbols: Set.t;
    accept: (int, 'o) Hashtbl.t;
    delta: (int * i, int) Hashtbl.t;
  }

  let empty () = {
    n_states = 0;
    symbols = Set.empty;
    accept = Hashtbl.create 16;
    delta = Hashtbl.create 16;
  }

  let new_state a =
    let n = a.n_states in
    a.n_states <- n + 1;
    n

  let set_accept a q o =
    Hashtbl.replace a.accept q o

  let is_accept a q =
    Hashtbl.mem a.accept q

  let add_transition a q s q' =
    if Hashtbl.mem a.delta (q, s) then
      false
    else
      (Hashtbl.add a.delta (q, s) q';
      a.symbols <- Set.add s a.symbols;
      true) 
  
  let add_transition_fill a q s =
    if Hashtbl.mem a.delta (q, s) then
      None
    else
      let q' = new_state a in
      ignore @@ add_transition a q s q';
      Some q'

  let read a q s =
    Hashtbl.find_opt a.delta (q, s)

  let to_string a =
    let padding = 1 + Misc.list_max (Set.to_list a.symbols) (fun x -> String.length (Input.to_string x)) in

    let rec f q s =
      if q = a.n_states then
        s
      else 
        let qs =
            let qs = string_of_int q in
            if is_accept a q then
              Misc.underline ^ qs ^ Misc.end_
            else
              qs in
        let s = 
          Printf.sprintf "%s%s %s\n" s qs @@
          Set.fold
            (fun s acc ->
              Printf.sprintf "%s|%s" acc @@
              Misc.pad_to padding @@
              match read a q s with
              | None -> "-"
              | Some q' -> string_of_int q')
            a.symbols
            "" in
        f (q+1) s in

    let s =
      Printf.sprintf "\\ %s\n" @@
      Set.fold
        (fun s acc ->
          Printf.sprintf "%s|%s" acc @@
          Misc.pad_to padding @@
          (Input.to_string s))
        a.symbols
        "" in

    f 0 s

  let print a =
    print_string (to_string a)
end