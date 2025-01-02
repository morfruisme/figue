open Figue

let () =
  let s = Rule.rules_to_string Rule.number in
  assert (s = "| 1 0 | 1 0 | 1 0 | _ Repeat | Repeat ");

  let x = "110_111_11" in
  assert (Rule.pass Rule.number x)