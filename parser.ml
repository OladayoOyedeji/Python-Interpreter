#use "parse_tree_type.ml";;

let rec process_scope scope delim =
  let rec f s acc =
  let token_list = tokenize s in
  let _ = print_tokens token_list in
  let _ = print_string "\n" in
  match token_list with
    Indent_tok n :: xs -> (match xs with
        [] -> (acc, " ")
      | token_list ->
        let _ = Printf.printf "%d %d\n" n scope in
        if n == scope then
          let e, t = stmt token_list in
          (* let _ =  *)
          (*   (match t with *)
          (*      [] -> (\* run_tree e symmtable *\) *)
          (*      () *)
          (*    | xs -> print_string "Error") *)
          (* in *)
          
          let _ = print_string delim in
          let _ = print_string " " in
          let s = "\n" ^  read_line () in
          let _ = print_string "\n" in
          f s (e::acc)
        else if n < scope then
          (acc, s)
        else
          raise (Failure "invalid indent")
    )
  | _ -> raise (Failure "Invalid statement")
and
  stmt toks =
    match toks with
        Id_tok name::Equ_tok::toks2 ->
        (
          let (e, toks3) = bool_expr toks2 in
          (Assgmt(name, e), toks3)
        )
      | If_tok::_ -> let (cond, toks) = cond_branch toks in
        (
          (Cond_stmt cond, toks)
        )
      | xs -> let e, tok = bool_expr xs in
      (Expr e, tok)
  and
    cond_branch toks =
    match toks with
      If_tok::toks -> let (b_expr, toks) = bool_expr toks in
      (
        match toks with
          Colon_tok::xs ->
          let expr, _ = process_scope (scope+1) "..." in
          (If_stmt (b_expr, expr), toks)
        | _ -> raise (Failure "Invalid syntax")
      )
    | _ -> raise (Failure "I dont even know!!")
  and
    arth_expr toks =
    let (t, toks1) = term toks in
    match toks1 with
      Basic_tok (x)::toks2 ->
      (
        let (e, toks3) = arth_expr toks2 in
        (Basic_Expr(x, t, e), toks3)
      )
    | _ -> (Term_Expr t, toks1)
  and
    bool_expr toks =
    let (a, toks) = arth_expr toks in
    match toks with
      Comp_tok (x)::tok2 ->
      (
        let (b, tok) = bool_expr tok2 in
        (Comparison_bool(x, a, b), tok)
      )
    | _ -> (Arth_Expr a, toks)

  and
    term toks =
    let (f, toks1) = factor toks in
    match toks1 with
      Inv_tok (x)::toks2 ->
      (
        let (t, toks3) = term toks2 in
        (Inv_Term(x, f, t), toks3)
      )
    | Exp_tok::toks2 ->
      (
        let (f1, toks3) = factor toks2 in
        (Exponential(f, f1), toks3)
      )
    | _ -> (Factor_Term f, toks1)
  and
    factor toks = 
    match toks with
      Literal_tok name::toks -> (Literal_Factor name, toks)
    | Id_tok name::toks -> (Variable_Factor name, toks)
    | Lparen_tok::toks1 ->
      (
        let (e, toks2) = bool_expr toks1 in
        match toks2 with
          Rparen_tok::toks3 -> (Paren_Expr_Factor(e), toks3)
        | _ -> raise (Failure "Missing Rparen")
      )
    | _ -> raise (Failure "Missing Id_tok or Lparen")
  in
  let _ = print_string delim in
  let _ = print_string " " in
  let s = "\n" ^  read_line () in
  let _ = print_string "\n" in
  f s []
;;

(* let create_parse_tree toks scope = *)
(*   match toks with *)
(*   Indent_tok scope1 :: toks -> *)
(*   if scope1 > scope || scope1 == -1 then *)
(*     raise (Failure "Invalid Indent") *)
(*   else if scope = scope1 then *)
(*     stmt toks *)
(*   else *)
(* ;; *)

(* let rec assign toks = *)
(*   match toks with *)
(*   Id_tok name::Equ_tok::toks2 -> *)
(*   ( *)
(*     let (b, toks3) = bool toks2 in *)
(*     (Assign(name, b), toks3) *)
(*   ) *)
  
(*   | xs -> let b, tok = bool xs in *)
(*     (Not_Assign b, tok) *)
(* and *)
(*   bool toks = *)
(*   let (e, toks) = expr toks in *)
(*   match toks with *)
(*     Is_Equ_tok::toks2 -> *)
(*     ( *)
(*       let (b, toks3) = bool toks2 in *)
(*       (Equ_bool(e, b), toks3) *)
(*     ) *)
(*   | Is_Neq_tok::toks2 -> *)
(*     ( *)
(*       let (b, toks3) = bool toks2 in *)
(*       (Nequ_bool(e, b), toks3) *)
(*     ) *)
(*   | Is_Less_tok::toks2 -> *)
(*     ( *)
(*       let (b, toks3) = bool toks2 in *)
(*       (Less_bool(e, b), toks3) *)
(*     ) *)
(*   | Is_Less_Equ_tok::toks2 -> *)
(*     ( *)
(*       let (b, toks3) = bool toks2 in *)
(*       (Less_Eq_bool(e, b), toks3) *)
(*     ) *)
(*   | Is_Great_tok::toks2 -> *)
(*     ( *)
(*       let (b, toks3) = bool toks2 in *)
(*       (Great_bool(e, b), toks3) *)
(*     ) *)
(*   | Is_Great_Equ_tok::toks2 -> *)
(*     ( *)
(*       let (b, toks3) = bool toks2 in *)
(*       (Great_Eq_bool(e, b), toks3) *)
(*     ) *)
(*   | _ -> (Expr e, toks) *)
(* and *)
(*   expr toks = *)
(*   let (t, toks1) = term toks in *)
(*   match toks1 with *)
(*     Minus_tok::toks2 -> *)
(*     ( *)
(*       let (e, toks3) = expr toks2 in *)
(*       (Minus_Expr(t, e), toks3) *)
(*     ) *)
(*   |  *)
(*     Plus_tok::toks2 -> *)
(*     ( *)
(*       let (e, toks3) = expr toks2 in *)
(*       (Plus_Expr(t, e), toks3) *)
(*     ) *)
(*   | _ -> (Term_Expr t, toks1) *)
(* and *)
(*   term toks = *)
(*   let (f, toks1) = factor toks in *)
(*   match toks1 with *)
(*     Mul_tok::toks2 -> *)
(*     ( *)
(*       let (t, toks3) = term toks2 in *)
(*       (Mul_Term(f, t), toks3) *)
(*     ) *)
(*   | Div_tok::toks2 -> *)
(*     ( *)
(*       let (t, toks3) = term toks2 in *)
(*       (Div_Term(f, t), toks3) *)
(*     ) *)
(*   | Exp_tok::toks2 -> *)
(*     ( *)
(*       let (f1, toks3) = term toks2 in *)
(*       (Exponential(f, f1), toks3) *)
(*     ) *)
(*   | _ -> (Factor_Term f, toks1) *)
(* and *)
(*   factor toks = match toks with *)
(*     Literal_tok name::toks -> (Literal_Factor name, toks) *)
(*   | Id_tok name::toks -> (Variable_Factor name, toks) *)
(*   | Lparen_tok::toks1 -> *)
(*     ( *)
(*       let (e, toks2) = bool toks1 in *)
(*       match toks2 with *)
(*         Rparen_tok::toks3 -> (Paren_Expr_Factor(e), toks3) *)
(*       | _ -> raise (Failure "Missing Rparen") *)
(*     ) *)
(*   | _ -> raise (Failure "Missing Id_tok or Lparen") *)
(* ;; *)
