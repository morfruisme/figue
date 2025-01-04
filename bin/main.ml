open Figue
open Lexer

module A = Automata.Make (Int)

let () =
  let a = A.empty () in
  ignore @@ A.add_transition_fill a 0 0;
  ignore @@ A.add_transition_fill a 0 10;
  ignore @@ A.add_transition_fill a 1 101;
  A.accept a 0 None;
  A.accept a 2 None;
  A.print a;

  Lexer.print Lexer.lexer