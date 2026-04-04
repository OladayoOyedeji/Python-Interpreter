#use "parser.ml";;

let symmtable = Hashtbl.create 10;;

let rec python_interpreter symmtable =
  let _ = print_string ">>>" in
  let _ = print_string " " in
  let s = read_line () in
  (* let s = "\n" ^ s in *)

  let token_list = tokenize s in
  (* let _ = print_tokens token_list in *)
  let e, xs = stmt token_list 0 in

  let _ = run_tree e symmtable in

  python_interpreter symmtable
;;

python_interpreter symmtable;;
