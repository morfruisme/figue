let underline = "\x1B[4m"
let end_ = "\x1B[0m"

let list_max l f =
    List.fold_left (fun m x -> max m (f x)) 0 l

let pad_to n s =
  let l = String.length s in
  if l < n then
    String.make (n-l) ' ' ^ s
  else
    s