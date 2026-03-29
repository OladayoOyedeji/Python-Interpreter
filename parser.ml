#use "parse_tree_type.ml";;

let rec assign toks =
  match toks with
  Id_tok name::Equ_tok::toks2 ->
  (
    let (b, toks3) = bool toks2 in
    (Assign(name, b), toks3)
  )
  
  | xs -> let b, tok = bool xs in
    (Not_Assign b, tok)
and
  bool toks =
  let (e, toks) = expr toks in
  match toks with
    Is_Equ_tok::toks2 ->
    (
      let (b, toks3) = bool toks2 in
      (Equ_bool(e, b), toks3)
    )
  | Is_Neq_tok::toks2 ->
    (
      let (b, toks3) = bool toks2 in
      (Nequ_bool(e, b), toks3)
    )
  | Is_Less_tok::toks2 ->
    (
      let (b, toks3) = bool toks2 in
      (Less_bool(e, b), toks3)
    )
  | Is_Less_Equ_tok::toks2 ->
    (
      let (b, toks3) = bool toks2 in
      (Less_Eq_bool(e, b), toks3)
    )
  | Is_Great_tok::toks2 ->
    (
      let (b, toks3) = bool toks2 in
      (Great_bool(e, b), toks3)
    )
  | Is_Great_Equ_tok::toks2 ->
    (
      let (b, toks3) = bool toks2 in
      (Great_Eq_bool(e, b), toks3)
    )
  | _ -> (Expr e, toks)
and
  expr toks =
  let (t, toks1) = term toks in
  match toks1 with
    Minus_tok::toks2 ->
    (
      let (e, toks3) = expr toks2 in
      (Minus_Expr(t, e), toks3)
    )
  | 
    Plus_tok::toks2 ->
    (
      let (e, toks3) = expr toks2 in
      (Plus_Expr(t, e), toks3)
    )
  | _ -> (Term_Expr t, toks1)
and
  term toks =
  let (f, toks1) = factor toks in
  match toks1 with
    Mul_tok::toks2 ->
    (
      let (t, toks3) = term toks2 in
      (Mul_Term(f, t), toks3)
    )
  | Div_tok::toks2 ->
    (
      let (t, toks3) = term toks2 in
      (Div_Term(f, t), toks3)
    )
  | Exp_tok::toks2 ->
    (
      let (f1, toks3) = term toks2 in
      (Exponential(f, f1), toks3)
    )
  | _ -> (Factor_Term f, toks1)
and
  factor toks = match toks with
    Literal_tok name::toks -> (Literal_Factor name, toks)
  | Id_tok name::toks -> (Variable_Factor name, toks)
  | Lparen_tok::toks1 ->
    (
      let (e, toks2) = bool toks1 in
      match toks2 with
        Rparen_tok::toks3 -> (Paren_Expr_Factor(e), toks3)
      | _ -> raise (Failure "Missing Rparen")
    )
  | _ -> raise (Failure "Missing Id_tok or Lparen")
;;
