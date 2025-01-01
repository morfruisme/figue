open Rule

type lexeme =
| Var | Const
| Si | Sinon
| TantQue
| Rien | Fin
| Plus | Moins | Mult | Div

type lex_repr =
| Keyword of (lexeme * string)
| Word of (lexeme * rule list)

let lexemes =
  let keywords = [
    (Si, "si"); (Sinon, "sinon");
    (TantQue, "tant que");
    (Rien, "rien"); (Fin, "fin");
    (Plus, "+"); (Moins, "-"); (Mult, "*"); (Div, "/");
  ]
  and words = [] in
  List.map (fun x -> Keyword x) keywords @ List.map (fun x -> Word x) words

type ('a, 'b) automata = {
  mutable n_states: int;
  accept: (int, 'a) Hashtbl.t;
  delta: (int * 'b, int) Hashtbl.t;
}

let add_transition a q s =
  match Hashtbl.find_opt a.delta (q, s) with
  | Some q' -> q'
  | None ->
    let q' = a.n_states in
    Hashtbl.add a.delta (q, s) q';
    a.n_states <- a.n_states + 1;
    q'

let add_keyword lexer l =
  let f q i s =
    if i = String.length s then
      Hashtbl.add lexer.accept q l;
    add_transition lexer q (Char s.[i]) in
  f 0 0

(*let add_word lexer l =
  let rec f q rl =
    match rl with
    | [] -> ()
    | r::rl ->
      () in
  f 0*)

let lexer: (lexeme, char) automata = {
  n_states = 0;
  accept = Hashtbl.create 16;
  delta = Hashtbl.create 16;
}