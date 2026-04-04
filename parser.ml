#use "parse_tree_type.ml";;

let rev list =
  let rec f list revlist = match list with
      [] -> revlist
    | x::xs -> f xs (x::revlist)
  in
  f list []
;;

let rec run_tree parse_tree symmtable =
  let rec eval_stmt stmt = match stmt with
      Cond_stmt (x) -> eval_cond x
    | Assgmt (v, e) ->
      let _ = Hashtbl.add symmtable v (eval_bool_expr e) in
          ()
    | Expr (e) ->
      let _ = print_literal (eval_bool_expr e) in
      let _ = print_string "\n" in
      ()
  and
    eval_cond cond = match cond with
      If_stmt (b, s) -> let bool_expr = literal_bool (eval_bool_expr b) in
      if bool_expr then
        let rec eval_stmts s = match s with
            [] -> ()
          | s::xs -> let _ = eval_stmt s in
            eval_stmts xs
        in
        eval_stmts s
      else
        ()
  and
    eval_bool_expr bool_expr = match bool_expr with
      Arth_Expr (a) -> eval_arth_expr a
    | Comparison_bool (op, a, b) -> compare op (eval_arth_expr a) (eval_bool_expr b)
  and
    eval_arth_expr arth_expr = match arth_expr with
      Term_Expr (t) -> eval_term t
    | Basic_Expr (op, t, a) -> evaluate_basic op (eval_term t) (eval_arth_expr a)
  and
    eval_term term = match term with
      Factor_Term (f) -> eval_factor f
    | Inv_Term (op, f, t) -> evaluate_inv op (eval_factor f) (eval_term t)
    | Exponential (f, f1) -> evaluate_exp (eval_factor f) (eval_factor f1)
  and
    eval_factor factor = match factor with
      Literal_Factor (l) -> l
    | Variable_Factor (v) -> Hashtbl.find symmtable v
    | Paren_Expr_Factor (b) -> eval_bool_expr b
  in
  eval_stmt parse_tree
;;

(* let accept_string = *)
  

let rec process_scope scope =
  let rec f s acc =
    let tokens = tokenize s in
    (* let _ = print_tokens tokens in *)
    (* let _ = print_string "\n" in *)
    match tokens with
      Indent_tok n :: xs -> (match xs with
          [] -> (rev acc, [])
        | token_list ->
          (* let _ = Printf.printf "%d %d\n" n scope in *)
          if n == scope then
            let e, t = stmt token_list scope in

            (* let _ = print_tokens t in *)
            (
              match t with
                [] -> let _ = print_string "..." in
                let _ = print_string " " in
                let s = read_line () in
                let s = "\n" ^ s in
                (* let _ = print_string "\n" in *)
                f s (e::acc)
              | toks ->
                (
                  match toks with
                    Indent_tok n :: xs ->
                    if n = scope then
                      let ex, _ = stmt xs scope in
                      let _ = print_string "..." in
                      let _ = print_string " " in
                      let s = read_line () in
                      let s = "\n" ^ s in
                      (* let _ = print_string "\n" in *)
                      f s (ex::(e::acc))
                    else if n < scope && n <> -1 then
                      (* let _ = print_string "return " in *)
                      let _ = print_tokens toks in
                      let _ = print_string "\n" in
                      (rev (e::acc), toks)
                    else
                      raise (Failure "invalid indent")
                  | _ -> raise (Failure "Syntax Error")
                )
            )
          else if n < scope && n <> (-1) then
            (rev acc, tokens)
          else
            raise (Failure "Invalid indent")
      )
    | _ -> raise (Failure "Invalid statement")
  in
  let _ = print_string "..." in
  let _ = print_string " " in
  let s = read_line () in
  let s = "\n" ^ s in
  (* let _ = print_string "\n" in *)
  f s []
and
  stmt toks scope =
  match toks with
    Id_tok name::Equ_tok::toks2 ->
    (
      let (e, toks3) = bool_expr toks2 scope in
      (Assgmt(name, e), toks3)
    )
  | If_tok::_ -> let (cond, toks) = cond_branch toks scope in
    (
      (Cond_stmt cond, toks)
    )
  | xs -> let e, tok = bool_expr xs scope in
    (Expr e, tok)
and
  cond_branch toks scope =
  match toks with
    If_tok::toks -> let (b_expr, toks) = bool_expr toks scope in
    (
      match toks with
        Colon_tok::xs ->
        (* let _ = print_string "branch\n" in *)
        let expr, t = process_scope (scope+1) in
        (
          match xs with
            [] -> (If_stmt (b_expr, expr), t)
          | xs -> let b, x = stmt xs scope in
            (
              match x with
                [] -> (If_stmt (b_expr, b::expr), t)
              | _ -> raise (Failure "Invalid syntax")
            )
        )
      | _ -> raise (Failure "Invalid syntax")
    )
  | _ -> raise (Failure "I dont even know!!")
and
  arth_expr toks scope =
  let (t, toks1) = term toks scope in
  match toks1 with
    Basic_tok (x)::toks2 ->
    (
      let (e, toks3) = arth_expr toks2 scope in
      (Basic_Expr(x, t, e), toks3)
    )
  | _ -> (Term_Expr t, toks1)
and
  bool_expr toks scope =
  let (a, toks) = arth_expr toks scope in
  match toks with
    Comp_tok (x)::tok2 ->
    (
      let (b, tok) = bool_expr tok2 scope in
      (Comparison_bool(x, a, b), tok)
    )
  | _ -> (Arth_Expr a, toks)

and
  term toks scope =
  let (f, toks1) = factor toks scope in
  match toks1 with
    Inv_tok (x)::toks2 ->
    (
      let (t, toks3) = term toks2 scope in
      (Inv_Term(x, f, t), toks3)
    )
  | Exp_tok::toks2 ->
    (
      let (f1, toks3) = factor toks2 scope in
      (Exponential(f, f1), toks3)
    )
  | _ -> (Factor_Term f, toks1)
and
  factor toks scope = 
  match toks with
    Literal_tok name::toks -> (Literal_Factor name, toks)
  | Id_tok name::toks -> (Variable_Factor name, toks)
  | Lparen_tok::toks1 ->
    (
      let (e, toks2) = bool_expr toks1 scope in
      match toks2 with
        Rparen_tok::toks3 -> (Paren_Expr_Factor(e), toks3)
      | _ -> raise (Failure "Missing Rparen")
    )
  | _ -> raise (Failure "Missing Id_tok or Lparen")
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
