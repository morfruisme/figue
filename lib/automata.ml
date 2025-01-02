module type ToString = sig
  type t
  val to_string: t -> string
end

module type A = sig
  type o
  type i
  type t
  val empty: unit -> t
  val add_transition: t -> int -> i -> int -> bool
  val add_transition_fill: ?q': int option -> t -> int -> i -> bool
end

module Make
  (Output: ToString)
  (Input: sig
    type t
    include ToString with type t := t
    include Set.OrderedType with type t := t
  end)
  : A = struct

  module Set = Set.Make (Input)

  type o = Output.t
  type i = Input.t

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

  let add_transition a q s q' =
    if Hashtbl.mem a.delta (q, s) then
      false
    else
      (Hashtbl.add a.delta (q, s) q';
      a.alphabet <- Set.add s a.alphabet;
      true)
  
  let add_transition_fill ?(q' = None) a q s =
    let q' = match q' with
      | None -> new_state a
      | Some q' -> q' in
    add_transition a q s q'

  let to_string a =
    let rec f i s =
      if i = a.n_states then
        s
      else
        failwith "!" in
    f 0 ""

end