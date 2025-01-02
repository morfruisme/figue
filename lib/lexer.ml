type lexeme =
| Var of string | Const of string
| Si | Sinon
| TantQue
| Affectation
| Rien | Fin
| Plus | Moins | Mult | Div

type lex_repr =
| Keyword of (lexeme * string)
| Word of (lexeme * string)

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

let add_keyword lexer l s =
  let n = String.length s in
  let f q i =
    if i = n then
      Hashtbl.add lexer.accept q l;
    add_transition lexer q s.[i] in
  ignore (f 0 0)

let add_word lexer l =
  String.iter
    (fun c ->
      (if Hashtbl.mem lexer.delta (0, c) then
        (* pour l'instant eviter les majuscules et symboles, meilleur automate plus tard *)
        failwith "word alphabet cannot contain the first char of a keyword");
      let q = add_transition lexer 0 c in
      Hashtbl.add lexer.delta (q, c) q;
      Hashtbl.add lexer.accept q l)

let lexer_init lexemes =
  let lexer = {
    n_states = 0;
    accept = Hashtbl.create 16;
    delta = Hashtbl.create 16;
  } in

  List.iter 
    (fun l ->
      match l with
      | Keyword (l, s) -> add_keyword lexer l s
      | Word (l, s) -> add_word lexer l s)
    lexemes;
  
  lexer

let lexer_one lexer s i =
  let rec f q j =
    match Hashtbl.find_opt lexer.delta (q, s.[i+j]) with
    | None -> q, j
    | Some q' -> f q' (j+1) in
  
  let q, j = f 0 0 in
  match Hashtbl.find_opt lexer.accept q with
  | None -> failwith "reading error"
  | Some (Var _) -> Var (String.sub s i j), j
  | Some (Const _) -> Const (String.sub s i j), j
  | Some l -> l, j

let lexer_read lexer s =
  let n = String.length s in
  let rec f i acc =
    if i = n then
      acc
    else
      let l, j = lexer_one lexer s i in
      f (i+j) (l::acc) in
  f 0 []

let lexemes =
  let keywords = [
    (Si, "Si"); (Sinon, "Sinon");
    (TantQue, "Tant que");
    (Affectation, ":=");
    (Rien, "Rien"); (Fin, "Fin");
    (Plus, "+"); (Moins, "-"); (Mult, "*"); (Div, "/");
  ]
  (* trouver un autre moyen pcq pas joli *)
  and words = [
    (Var "", "abcdefghijklmnopqrstuvwxyz_");
    (Const "", "0123456789")
  ] in

  List.map (fun x -> Keyword x) keywords @ List.map (fun x -> Word x) words

let print_lexeme l =
  match l with
  | Var s -> Printf.sprintf "var %s" s
  | Const s -> Printf.sprintf "const %s" s
  | _ -> 
    Option.get @@ 
    List.find_map 
      (fun l' -> 
        match l' with
        | Keyword (l', s) when l = l' -> Some s
        | _ -> None)
      lexemes

let lexer = lexer_init lexemes