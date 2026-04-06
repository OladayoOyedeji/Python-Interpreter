#use "lexer.mml.ml";;

(* file: parse_tree_type.ml *)
type stmt = Cond_stmt of cond_branch
          (* | Cond_loop of cond_loop *)
          (* | *)
          | Assgmt of (string * bool_expr)
          | Expr of bool_expr
              
and
  cond_branch = If_stmt of (bool_expr * stmt list)
              | If_elif_stmt of (bool_expr * stmt list * cond_branch)
              | If_else_stmt of (bool_expr * stmt list * stmt list)
              | While_stmt of (bool_expr * stmt list)
(* and *)
(*   cond_loop = While_stmt of (expr * stmt) *)

and
  bool_expr = Arth_Expr of arth_expr
       (* Bool_ Expression *)
       (* | Equ_bool of (expr * expr) *)
       (* | Nequ_bool of (expr * expr) *)
       (* | Less_bool of (expr * expr) *)
       (* | Great_bool of (expr * expr) *)
       (* | Less_Eq_bool of (expr * expr) *)
       (* | Great_Eq_bool of (expr * expr) *)
       | Comparison_bool of (comparison_op * arth_expr * bool_expr)
and
  arth_expr = Term_Expr of term
       (* Arithmetic Expression *)
       (* | Plus_Expr of (term * expr) *)
       (* | Minus_Expr of (term * expr) *)
       | Basic_Expr of (basic_op * term * arth_expr)

and
  term = Factor_Term of factor
       (* | Mul_Term of (factor * term) *)
       (* | Div_Term of (factor * term) *)
       (* | Mod_Term of (factor * term) *)
       | Inv_Term of (inverse_op * factor * term)
       | Exponential of (factor * factor)
and
  factor = Literal_Factor of literal
         | Variable_Factor of string
         | Paren_Expr_Factor of bool_expr
         | List_Factor of bool_expr list
;;
    

(* type assign = Not_Assign of bool *)
(*             | Assign of (string * bool) *)
(* and *)
(*   bool = Expr of expr *)
(*        | Equ_bool of (expr * bool) *)
(*        | Nequ_bool of (expr * bool) *)
(*        | Less_bool of (expr * bool) *)
(*        | Great_bool of (expr * bool) *)
(*        | Less_Eq_bool of (expr * bool) *)
(*        | Great_Eq_bool of (expr * bool) *)
(* and *)
(*   expr = Term_Expr of term *)
(*        | Plus_Expr of (term * expr) *)
(*        | Minus_Expr of (term * expr) *)
(* and *)
(*   term = Factor_Term of factor *)
(*        | Mul_Term of (factor * term) *)
(*        | Div_Term of (factor * term) *)
(*        | Mod_Term of (factor * term) *)
(*        | Exponential of (factor * factor) *)
(* and *)
(*   factor = Literal_Factor of literal *)
(*          | Variable_Factor of string *)
(*          | Paren_Expr_Factor of bool *)
(* ;; *)

