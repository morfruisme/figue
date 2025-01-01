open Compiler.Rule

let () =
  List.iter (fun r -> Printf.printf "|"; print_rule r) number_rule;
  let x = "110_111_11" in
  Printf.printf "\n%b\n" @@ pass_rule number_rule x;
  ()