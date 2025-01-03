open Display

module type ToString = sig
  type t
  val to_string: t -> string
end

module type A = sig
  type o
  type i
  type set

  type t = private {
    mutable n_states: int;
    mutable alphabet: set;
    accept: (int, o) Hashtbl.t;
    delta: (int * i, int) Hashtbl.t;
  }

  val empty: unit -> t
  val new_state: t -> int
  val set_accept: t -> int -> o -> unit
  val is_accept: t -> int -> bool
  val add_transition: t -> int -> i -> int -> bool
  val add_transition_fill: t -> int -> i -> int option
  val read: t -> int -> i -> int option
  val to_string: t -> string
  val print: t -> unit
end

module Make
  (Output: ToString)
  (Input: sig
    type t
    include ToString with type t := t
    include Set.OrderedType with type t := t
  end)
  : A with type o = Output.t and type i = Input.t
  = struct

  module Set = Set.Make (Input)

  type o = Output.t
  type i = Input.t
  type set = Set.t

  type t = {
    mutable n_states: int;
    mutable alphabet: Set.t;
    accept: (int, o) Hashtbl.t;
    delta: (int * i, int) Hashtbl.t;
  }

  let empty () = {
    n_states = 0;
    alphabet = Set.empty;
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
      a.alphabet <- Set.add s a.alphabet;
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

  let list_max l f =
    List.fold_left (fun m x -> max m (f x)) 0 l

  let pad_to n s =
    let l = String.length s in
    if l < n then
      String.make (n-l) ' ' ^ s
    else
      s

  let to_string a =
    let padding = 1 + list_max (Set.to_list a.alphabet) (fun x -> String.length (Input.to_string x)) in

    let rec f q s =
      if q = a.n_states then
        s
      else
        let s =
          let qs = string_of_int q in
          Printf.sprintf "%s%s %s\n" s 
            (if is_accept a q then
              underline ^ qs ^ end_
            else
              qs) @@
          Set.fold
            (fun s acc ->
              Printf.sprintf "%s|%s" acc @@
              pad_to padding @@
              match read a q s with
              | None -> "-"
              | Some q' -> string_of_int q')
            a.alphabet
            "" in
        f (q+1) s in

    let s =
      Printf.sprintf "\\ %s\n" @@
      Set.fold
        (fun s acc ->
          Printf.sprintf "%s|%s" acc @@
          pad_to padding @@
          (Input.to_string s))
        a.alphabet
        "" in
    f 0 s

  let print a =
    print_string (to_string a)
end