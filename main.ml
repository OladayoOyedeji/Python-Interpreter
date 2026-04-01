#use "parser.ml";;

let symmtable = Hashtbl.create 10;;

let process_scope scope delim =
  let rec f s acc =
  let token_list = tokenize s in
  let _ = print_tokens token_list in
  let _ = print_string "\n" in
  match token_list with
    Indent_tok n :: xs -> (match xs with
        [] -> (acc, " ")
      | token_list ->
        let _ = Printf.printf "%d %d" n scope in
        if n == scope then
          let e, t = stmt token_list in
          (* let _ =  *)
          (*   (match t with *)
          (*      [] -> (\* run_tree e symmtable *\) *)
          (*      () *)
          (*    | xs -> print_string "Error") *)
          (* in *)
          
          let _ = print_string delim in
          let s = "\n" ^  read_line () in
          let _ = print_string "\n" in
          f s (e::acc)
        else if n < scope then
          (acc, s)
        else
          raise (Failure "invalid indent")
    )
  | _ -> raise (Failure "Invalid statement")
  in
  let _ = print_string delim in
  let s = "\n" ^  read_line () in
  let _ = print_string "\n" in
  f s [] scope
;;


process_scope s [] 0;;
