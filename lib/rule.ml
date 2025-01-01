(* temporaire, a remplacer par lang regulier *)

type rule =
| Char of char
| Or of rule list
| Repeat

let rule_of_string s =
  let rec f i l =
    if i = String.length s then
      l
    else
      f (i+1) (Char s.[i]::l) in
  Or (f 0 [])

let number_rule =
  [rule_of_string "01"; rule_of_string "01"; rule_of_string "01"; Or [Char '_'; Repeat]; Repeat]

let rec print_rule r =
  match r with
  | Char c -> Printf.printf "%c " c
  | Or rl -> List.iter print_rule rl
  | Repeat -> Printf.printf "Repeat "

let pass_rule rl s =
  (* repeat if bool is true *)
  let exception Pass of bool in

  let rec f r i =
    match r with
    | Char c when c = s.[i] -> raise (Pass false)
    | Or rl -> List.iter (fun r -> f r i) rl
    | Repeat -> raise (Pass true)
    | _ -> () in
  
  let rec g rl' i =
    if i = String.length s then
      true
    else
      match rl' with
      | [] -> g rl i
      | r::rl' ->
        try f r i; false with
        | Pass true -> g rl i
        | Pass false -> g rl' (i+1) in
    
  g rl 0

