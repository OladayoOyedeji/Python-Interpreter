#use "lexer.mml.ml";;

(* file: parse_tree_type.ml *)
type assign = Not_Assign of bool
            | Assign of (string * bool)
and
  bool = Expr of expr
       | Equ_bool of (expr * bool)
       | Nequ_bool of (expr * bool)
       | Less_bool of (expr * bool)
       | Great_bool of (expr * bool)
       | Less_Eq_bool of (expr * bool)
       | Great_Eq_bool of (expr * bool)
and
  expr = Term_Expr of term
       | Plus_Expr of (term * expr)
       | Minus_Expr of (term * expr)
and
  term = Factor_Term of factor
       | Mul_Term of (factor * term)
       | Div_Term of (factor * term)
       | Mod_Term of (factor * term)
       | Exponential of (factor * factor)
and
  factor = Literal_Factor of literal
         | Variable_Factor of string
         | Paren_Expr_Factor of bool
;;

