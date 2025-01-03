let end_ = "\x1B[0m"

let style code s =
  code ^ s ^ end_

let bold = style "\x1B[1m"
let italic = style "\x1B[3m"
let underline = style "\x1B[4m"

let list_max l f =
    List.fold_left (fun m x -> max m (f x)) 0 l

let pad_to n s =
  let l = String.length s in
  if l < n then
    String.make (n-l) ' ' ^ s
  else
    s

let explode s =
  List.rev @@
  String.fold_left (fun acc c -> c::acc) [] s