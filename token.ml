(* File: token.ml *)

exception Error;;

type literal = Int_tok of int
             | Float_tok of float
             | Bool_tok of int
             | String_tok of string
;;

type comparison_op = Is_Equ_tok
                   | Is_Neq_tok
                   | Is_Less_tok
                   | Is_Less_Equ_tok
                   | Is_Great_tok
                   | Is_Great_Equ_tok
;;

type basic_op = Plus_tok
           | Minus_tok
;;

type inverse_op = Mul_tok
           | Div_tok
           | Mod_tok
;;

type token = Literal_tok of literal
           | Id_tok of string
           | Basic_tok of basic_op
           | Inv_tok of inverse_op
           | Exp_tok
           | Comp_tok of comparison_op
           | Indent_tok of int
           | Equ_tok
           | Colon_tok
           | If_tok
           | Else_tok
           | Lcomment_tok
           | Rcomment_tok
           | Lparen_tok
           | Rparen_tok
;;

(* Assuming it starts from '\n' *)
let count_indent s =
  let rec f i =
    if i < String.length s then
      if s.[i] = ' '
      then 1 + f (i + 1)
      else if s.[i] = '\t'
      then 4 + f (i + 1)
      else
        f (i+1)
    else
      0
  in
  f 0
;;

exception IgnoreCase;;

(* Write a print_token function. For instance print_token (Int_tok 5)
   prints
   Int_tok 5
   in the console window.
*)

let print_literal literal = match literal with
        Int_tok (x) -> let _ = Printf.printf "Int_tok %d" x in
        ()
      | Float_tok (x) -> let _ = Printf.printf "Float_tok %f" x in
        ()
      | Bool_tok (x) -> if x == 1 then
          print_string "True"
        else
          print_string "False"
      | String_tok (x) -> print_string x
        
;;

let print_token t = match t with
  Literal_tok (l) -> print_literal l
  | Id_tok x -> let _ = Printf.printf "Id tok %s" x in
    ()
  | Basic_tok (b) -> (match b with
        Plus_tok -> print_string "Plus_tok +"
      | Minus_tok -> print_string "Minus_tok -")
  | Inv_tok (i) -> (match i with
        Mul_tok -> print_string "Mul_tok *"
      | Div_tok -> print_string "Div_tok /"
      | Mod_tok -> print_string "Mod_tok %"
    )
  | Exp_tok -> print_string "Exp_tok **"
  | Comp_tok (c) -> (match c with
        Is_Equ_tok -> print_string "Is_Equ_tok =="
      | Is_Neq_tok -> print_string "Is_Neq_tok !="
      | Is_Less_tok -> print_string "Is_Less_tok <"
      | Is_Less_Equ_tok -> print_string "Is_Less_Equ_tok <="
      | Is_Great_tok -> print_string "Is_Great_tok >"
      | Is_Great_Equ_tok -> print_string "Is_Great_Eq_tok >=")
  | Colon_tok -> print_string "Colon_tok"
  | Equ_tok -> print_string "Eq_tok ="
  | Indent_tok scope -> Printf.printf "Indent tok scope: %d" scope
  | If_tok -> print_string "If_tok if"
  | Else_tok -> print_string "Else_tok else"
  | Lcomment_tok -> print_string "comment (*"
  | Rcomment_tok -> print_string "comment *)"
  | Lparen_tok -> print_string "LParen_tok ("
  | Rparen_tok -> print_string "RParen_tok )"
;;

(* Write a print_tokens function. For instance print_tokens [Int_tok 5;
   Float 3.1; Id_tok "num_heads"; Plus_tok] prints
   [Int_tok 5, Float 3.1, Id_tok "num_heads", Plus_tok]
   on the console window.
*)
let print_tokens tokens =
  let rec f tokens delim = match tokens with
      [] -> let _ = print_string "]" in
      ()
    | x::xs -> let _ = print_string delim in
      let _ = print_token x in
      f xs ", "
  in
  f tokens "["
;;
