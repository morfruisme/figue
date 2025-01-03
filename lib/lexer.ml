module AChar = struct
  type t = char
  let to_string = Char.escaped
  let compare = Char.compare
end

module A = Automata.Make (AChar)


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

let add_keyword lexer l s =
  let n = String.length s in
  let f q i =
    if i = n then
      A.set_accept lexer q l;
    A.add_transition_fill lexer q s.[i] in
  ignore (f 0 0)

let add_word lexer l =
  String.iter
    (fun c ->
      let q = A.add_transition_fill lexer 0 c in
      match q with
      | None ->
        (* ne peut pas commencer par un symbole déjà utilisé *)
        failwith @@ Printf.sprintf "some keyword may start with %s" (AChar.to_string c)
      | Some q ->
        ignore @@ A.add_transition lexer q c q;
        A.set_accept lexer q l)

let init lexemes =
  let lexer = A.empty () in

  List.iter 
    (fun l ->
      match l with
      | Keyword (l, s) -> add_keyword lexer l s
      | Word (l, s) -> add_word lexer l s)
    lexemes;
  
  lexer

let read_one lexer s i =
  let rec f q j =
    match A.read lexer q s.[i+j] with
    | None -> q, j
    | Some q' -> f q' (j+1) in
  
  let q, j = f 0 0 in
  match A.accept lexer q with
  | None -> failwith "reading error"
  | Some (Var _) -> Var (String.sub s i j), j
  | Some (Const _) -> Const (String.sub s i j), j
  | Some l -> l, j

let read lexer s =
  let n = String.length s in
  let rec f i acc =
    if i = n then
      acc
    else
      let l, j = read_one lexer s i in
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

let lexer = init lexemes