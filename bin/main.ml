open Figue.Automata

module Int = struct
  type t = int
  let to_string = string_of_int
  let compare = compare
end

module A = Make (Int)

let () =
  let a = A.empty () in
  ignore @@ A.add_transition_fill a 0 0;
  ignore @@ A.add_transition_fill a 0 10;
  ignore @@ A.add_transition_fill a 1 101;
  A.set_accept a 0 None;
  A.set_accept a 2 None;
  A.print a