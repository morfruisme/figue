open Figue.Lexer

let () =
  let l = lexer_read lexer "a := 1" in
  List.iter (fun l -> print_string (print_lexeme l)) l