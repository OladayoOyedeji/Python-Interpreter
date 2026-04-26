#use "parser.ml";;

let symmtable = Hashtbl.create 10;;

let rec python_interpreter symmtable =
  let _ = print_string "\027[31m>>>\027[0m" in
  let _ = print_string " " in
  let s = "\n" ^ read_line () in
  (* let s = "\n" ^ s in *)

  let token_list = tokenize s in
  (* let _ = print_tokens token_list in *)
  match token_list with
    Indent_tok n :: token_list ->
    if n <> 0 then
      raise (Failure "Invalid Indent")
    else
      (
        (* let _ = print_tokens token_list in *)
        try 
          match token_list with
            [] -> python_interpreter symmtable
          | token_list ->
            (* let _ = print_tokens token_list in *)
            let e, xs = stmt token_list 0 in
            
            let _ = run_tree e symmtable
                
            in
            let _ = match xs with
                Indent_tok n::xs ->
                if n <> 0 then
                  raise (Failure "Invalid indent")
                else
                  (match xs with
                     [] -> ()
                   | xs -> 
                     let e, xs = stmt xs 0 in
                     (match xs with
                        [] -> let _ = run_tree e symmtable in
                        ()
                      | _ -> raise (Failure "Invalid syntax")
                     )
                  )
            | [] -> ()
            | _ -> raise (Failure "Invalid syntax")
            in
            
            python_interpreter symmtable
        with
        | Failure s -> let _ = print_string (s ^ "\n") in
          python_interpreter symmtable
      )
  | _ -> raise (Failure "Invalid syntax")
;;

python_interpreter symmtable;;
