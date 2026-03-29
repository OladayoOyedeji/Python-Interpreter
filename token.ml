(* File: token.ml *)

exception Error;;

type literal = Int_tok of int
             | Float_tok of float
             | Bool_tok of int
;;

type token = Literal_tok of literal
           | String_tok of string
           | Id_tok of string
           | Plus_tok
           | Minus_tok
           | Mul_tok
           | Div_tok
           | Mod_tok
           | Exp_tok
           | Is_Equ_tok
           | Is_Neq_tok
           | Is_Less_tok
           | Is_Less_Equ_tok
           | Is_Great_tok
           | Is_Great_Equ_tok
           | Equ_tok
           | If_tok
           | Else_tok
           | Lcomment_tok
           | Rcomment_tok
           | Lparen_tok
           | Rparen_tok
;;

exception IgnoreCase;;

(* Write a print_token function. For instance print_token (Int_tok 5)
   prints
   Int_tok 5
   in the console window.
*)

let print_literal literal = match literal with
        Int_tok (x) -> let _ = Printf.printf "%d" x in
        ()
      | Float_tok (x) -> let _ = Printf.printf "%f" x in
        ()
      | Bool_tok (x) -> if x == 1 then
          print_string "True"
        else
          print_string "False"
        
;;

let print_token t = match t with
  Literal_tok (l) -> print_literal l
  | String_tok x -> let _ = Printf.printf "String_tok %s" x in
    ()
  | Id_tok x -> let _ = Printf.printf "Id tok %s" x in
    ()
  | Plus_tok -> print_string "Plus_tok +"
  | Minus_tok -> print_string "Minus_tok -"
  | Mul_tok -> print_string "Mul_tok *"
  | Div_tok -> print_string "Div_tok /"
  | Mod_tok -> print_string "Mod_tok %"
  | Exp_tok -> print_string "Exp_tok **"
  | Is_Equ_tok -> print_string "Is_Equ_tok =="
  | Is_Neq_tok -> print_string "Is_Neq_tok !="
  | Is_Less_tok -> print_string "Is_Less_tok <"
  | Is_Less_Equ_tok -> print_string "Is_Less_Equ_tok <="
  | Is_Great_tok -> print_string "Is_Great_tok >"
  | Is_Great_Equ_tok -> print_string "Is_Great_Eq_tok >="
  | Equ_tok -> print_string "Eq_tok ="
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

let evaluate_literal tuple = let x,op,y = tuple in
  match (x, y) with
    Float_tok (x), Float_tok (y) ->  (match op with
        Plus_tok -> Float_tok(x +. y)
      | Minus_tok -> Float_tok(x -. y)
      | Mul_tok -> Float_tok(x *. y)
      | Div_tok -> Float_tok(x /. y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= y))
      | _ -> raise (Failure "Invalid op"))

  | Float_tok (x), Int_tok (y) ->  (match op with
        Plus_tok -> Float_tok(x +. (float_of_int y))
      | Minus_tok -> Float_tok(x -. (float_of_int y))
      | Mul_tok -> Float_tok(x *. (float_of_int y))
      | Div_tok -> Float_tok(x /. (float_of_int y))
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = (float_of_int y)))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> (float_of_int y)))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < (float_of_int y)))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > (float_of_int y)))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= (float_of_int y)))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= (float_of_int y)))
      | _ -> raise (Failure "Invalid op"))
    
  | Float_tok (x), Bool_tok (y) -> (match op with
        Plus_tok -> Float_tok(x +. (float_of_int y))
      | Minus_tok -> Float_tok(x -. (float_of_int y))
      | Mul_tok -> Float_tok(x *. (float_of_int y))
      | Div_tok -> Float_tok(x /. (float_of_int y))
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = (float_of_int y)))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> (float_of_int y)))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < (float_of_int y)))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > (float_of_int y)))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= (float_of_int y)))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= (float_of_int y)))
      | _ -> raise (Failure "Invalid op"))
    
  | Int_tok (x), Int_tok (y) -> (match op with
        Plus_tok -> Int_tok (x + y)
      | Minus_tok -> Int_tok (x - y)
      | Mul_tok -> Int_tok (x * y)
      | Div_tok -> Int_tok (x / y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= y))
      | _ -> raise (Failure "Invalid op"))
    
  | Int_tok (x), Float_tok (y) ->  (match op with
        Plus_tok -> Float_tok((float_of_int x) +. y)
      | Minus_tok -> Float_tok((float_of_int x) -. y)
      | Mul_tok -> Float_tok((float_of_int x) *. y)
      | Div_tok -> Float_tok((float_of_int x) /. y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int((float_of_int x) = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int((float_of_int x) <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int((float_of_int x) < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int((float_of_int x) > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int((float_of_int x) <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int((float_of_int x) >= y))
      | _ -> raise (Failure "Invalid op"))
    
  | Int_tok (x), Bool_tok (y) -> (match op with
        Plus_tok -> Int_tok (x + y)
      | Minus_tok -> Int_tok (x - y)
      | Mul_tok -> Int_tok (x * y)
      | Div_tok -> Int_tok (x / y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= y))
      | _ -> raise (Failure "Invalid op"))
    
  | Bool_tok (x), Bool_tok (y) -> (match op with
        Plus_tok -> Int_tok (x + y)
      | Minus_tok -> Int_tok (x - y)
      | Mul_tok -> Int_tok (x * y)
      | Div_tok -> Int_tok (x / y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= y))
      | _ -> raise (Failure "Invalid op"))
    
  | Bool_tok (x), Float_tok (y) ->  (match op with
        Plus_tok -> Float_tok((float_of_int x) +. y)
      | Minus_tok -> Float_tok((float_of_int x) -. y)
      | Mul_tok -> Float_tok((float_of_int x) *. y)
      | Div_tok -> Float_tok((float_of_int x) /. y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int(float_of_int x = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(float_of_int x <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int(float_of_int x < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int(float_of_int x > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(float_of_int x <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(float_of_int x >= y))
      | _ -> raise (Failure "Invalid op"))
    
  | Bool_tok (x), Int_tok (y) -> (match op with
        Plus_tok -> Int_tok (x + y)
      | Minus_tok -> Int_tok (x - y)
      | Mul_tok -> Int_tok (x * y)
      | Div_tok -> Int_tok (x / y)
      | Is_Equ_tok -> Bool_tok (Bool.to_int(x = y))
      | Is_Neq_tok -> Bool_tok (Bool.to_int(x <> y))
      | Is_Less_tok -> Bool_tok (Bool.to_int(x < y))
      | Is_Great_tok -> Bool_tok (Bool.to_int(x > y))
      | Is_Less_Equ_tok -> Bool_tok (Bool.to_int(x <= y))
      | Is_Great_Equ_tok -> Bool_tok (Bool.to_int(x >= y))
      | _ -> raise (Failure "Invalid op"))
    
;;
