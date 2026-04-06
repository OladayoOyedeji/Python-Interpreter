(* File: token.ml *)

exception Error;;
let print_list print_elem lst =
  print_string "[";
  let rec aux = function
    | [] -> ()
    | [x] -> print_elem x
    | x :: xs ->
        print_elem x;
        print_string ", ";
        aux xs
  in
  aux lst;
  print_string "]"
;;

let int_pow x n =
  let rec f squares n p =
    if n = 0 then p
    else if n mod 2 = 1 then
      f (squares * squares) (n/2) (p * squares)
    else
      f (squares * squares) (n/2) p
  in
  f x n 1
;;

let (%) x y = let r = x mod y in if r < 0 then r + y else r;;

type literal = Int_tok of int
             | Float_tok of float
             | Bool_tok of int
             | String_tok of string
             | Iterables_tok of iterable
             (* | None *)
             (* | Lists of (literal array) *)
             (* | Tuples of (literal array) *)
and
  iterable = List_tok of literal list
  
;;

let literal_bool lit = match lit with
    Int_tok x | Bool_tok x -> (x <> 0)
  | Float_tok x -> (x <> 0.0)
  | String_tok s -> (s <> "")
  | Iterables_tok t -> (match t with
    List_tok l -> (l = []))
  (* | None -> false *)
  (* | Lists (l) -> Array.length l == 0 *)
  (* | Tuples (t) -> Array.length t == 0 *)
;;

type comparison_op = Is_Equ_tok
                   | Is_Neq_tok
                   | Is_Less_tok
                   | Is_Less_Equ_tok
                   | Is_Great_tok
                   | Is_Great_Equ_tok
                   | Membership_tok
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
           | Elif_tok
           | While_tok 
           | Lcomment_tok
           | Rcomment_tok
           | Comma_tok
           | Lbracket_tok
           | Rbracket_tok
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

(* let to_float lit = match lit with *)
  

let rec compare op a b =
  match op with
    Is_Equ_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        if i = i1 then Bool_tok 1 else Bool_tok 0
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        if (float_of_int i) = f then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), Float_tok (f1) ->
        if f = f1 then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        if f = (float_of_int i1) then Bool_tok 1 else Bool_tok 0
      | String_tok (s), String_tok (s1) ->
        if s = s1 then Bool_tok 1 else Bool_tok 0
      | Iterables_tok l0, Iterables_tok l1 ->
        let rec eq l0 l1 = match (l0, l1) with
          [], [] -> Bool_tok 1
          | x::xs, y::ys -> if literal_bool (compare Is_Equ_tok x y) then
              eq xs ys
            else
              Bool_tok 0
          | _, _ -> Bool_tok 0
        in
        let List_tok l0 = l0 in
        let List_tok l1 = l1 in
        eq l0 l1
      | _, _ -> Bool_tok 0
    )
  | Is_Neq_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        if i <> i1 then Bool_tok 1 else Bool_tok 0
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        if (float_of_int i) <> f then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), Float_tok (f1) ->
        if f <> f1 then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        if f <> (float_of_int i1) then Bool_tok 1 else Bool_tok 0
      | String_tok (s), String_tok (s1) ->
        if s <> s1 then Bool_tok 1 else Bool_tok 0
      | Iterables_tok l0, Iterables_tok l1 ->
        (match  compare Is_Equ_tok a b with
          Bool_tok b -> Bool_tok ((b + 1) % 2)
        | _ -> raise (Failure "What?"))
      | _, _ -> Bool_tok 0
    )
  | Is_Less_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        if i < i1 then Bool_tok 1 else Bool_tok 0
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        if (float_of_int i) < f then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), Float_tok (f1) ->
        if f < f1 then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        if f < (float_of_int i1) then Bool_tok 1 else Bool_tok 0
      | String_tok (s), String_tok (s1) ->
        if s < s1 then Bool_tok 1 else Bool_tok 0
      | _, _ -> raise (Failure "miss matching")
    )
  | Is_Less_Equ_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        if i <= i1 then Bool_tok 1 else Bool_tok 0
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        if (float_of_int i) <= f then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), Float_tok (f1) ->
        if f <= f1 then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        if f <= (float_of_int i1) then Bool_tok 1 else Bool_tok 0
      | String_tok (s), String_tok (s1) ->
        if s <= s1 then Bool_tok 1 else Bool_tok 0
      | _, _ -> raise (Failure "miss matching")
    )
  | Is_Great_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        if i > i1 then Bool_tok 1 else Bool_tok 0
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        if (float_of_int i) > f then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), Float_tok (f1) ->
        if f > f1 then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        if f > (float_of_int i1) then Bool_tok 1 else Bool_tok 0
      | String_tok (s), String_tok (s1) ->
        if s > s1 then Bool_tok 1 else Bool_tok 0
      | _, _ -> raise (Failure "miss matching")
    )
  | Is_Great_Equ_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        if i >= i1 then Bool_tok 1 else Bool_tok 0
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        if (float_of_int i) >= f then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), Float_tok (f1) ->
        if f >= f1 then Bool_tok 1 else Bool_tok 0
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        if f >= (float_of_int i1) then Bool_tok 1 else Bool_tok 0
      | String_tok (s), String_tok (s1) ->
        if s >= s1 then Bool_tok 1 else Bool_tok 0
      | _, _ -> raise (Failure "miss matching")
    )
  | Membership_tok ->
    (
      match b with
        Iterables_tok (l) ->
        (
          (match l with
            List_tok l ->
            let rec elementof x list = match list with
                [] -> false
              | y::xs ->
                if literal_bool (compare Is_Neq_tok x y) then
                  elementof x xs
                else
                  true
            in
            if elementof a l then Bool_tok 1 else Bool_tok 0
          )
        )
      | _ -> raise (Failure "Is not Iterable")
    )
;;

let evaluate_basic op a b =
  match op with
    Plus_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        Int_tok (i + i1)
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        Float_tok ((float_of_int i) +. f)
      | Float_tok (f), Float_tok (f1) ->
        Float_tok (f +. f1)
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        Float_tok (f +. (float_of_int i1))
      | String_tok (s), String_tok (s1) ->
        String_tok (s ^ s1)
      | _, _ -> raise (Failure "miss matching")
    )
  | Minus_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        Int_tok (i - i1)
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        Float_tok ((float_of_int i) -. f)
      | Float_tok (f), Float_tok (f1) ->
        Float_tok (f -. f1)
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        Float_tok (f -. (float_of_int i1))
      | _, _ -> raise (Failure "miss matching")
    )
;;

let evaluate_inv op a b =
  match op with
    Mul_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        Int_tok (i * i1)
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        Float_tok ((float_of_int i) *. f)
      | Float_tok (f), Float_tok (f1) ->
        Float_tok (f *. f1)
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        Float_tok (f *. (float_of_int i1))
      | _, _ -> raise (Failure "miss matching")
    )
  | Div_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        Int_tok (i / i1)
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        Float_tok ((float_of_int i) /. f)
      | Float_tok (f), Float_tok (f1) ->
        Float_tok (f /. f1)
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        Float_tok (f /. (float_of_int i1))
      | _, _ -> raise (Failure "miss matching")
    )
  | Mod_tok ->
    (
      match (a, b) with
        (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
        Int_tok (i % i1)
      | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
        Float_tok (mod_float (float_of_int i)  f)
      | Float_tok (f), Float_tok (f1) ->
        Float_tok (mod_float f f1)
      | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
        Float_tok (mod_float f (float_of_int i1))
      | _, _ -> raise (Failure "miss matching")
    )
;;

let evaluate_exp a b = match (a, b) with
    (Int_tok (i) | Bool_tok (i)), (Int_tok (i1) | Bool_tok (i1)) ->
    Int_tok (int_pow i i1)
  | (Int_tok (i) | Bool_tok (i)), Float_tok (f) ->
    Float_tok ((float_of_int i) ** f)
  | Float_tok (f), Float_tok (f1) ->
    Float_tok (f ** f1)
  | Float_tok (f), (Int_tok (i1) | Bool_tok (i1)) ->
    Float_tok (f ** (float_of_int i1))
  | _, _ -> raise (Failure "miss matching")
;;


exception IgnoreCase;;

(* Write a print_token function. For instance print_token (Int_tok 5)
   prints
   Int_tok 5
   in the console window.
*)

let rec print_literal literal = match literal with
        Int_tok (x) -> let _ = Printf.printf "%d" x in
        ()
      | Float_tok (x) -> let _ = Printf.printf "%f" x in
        ()
      | Bool_tok (x) -> if x == 1 then
          print_string "True"
        else
          print_string "False"
      | String_tok (x) -> print_string x
      | Iterables_tok t -> (match t with
            List_tok l -> print_list print_literal l)
                          
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
      | Is_Great_Equ_tok -> print_string "Is_Great_Eq_tok >="
      | Membership_tok -> print_string "member in")
  | Colon_tok -> print_string "Colon_tok"
  | Equ_tok -> print_string "Eq_tok ="
  | Indent_tok scope -> Printf.printf "Indent tok scope: %d" scope
  | If_tok -> print_string "If_tok if"
  | Else_tok -> print_string "Else_tok else"
  | Elif_tok -> print_string "Elif_tok elif"
  | While_tok -> print_string "While_tok while"
  | Lcomment_tok -> print_string "comment (*"
  | Rcomment_tok -> print_string "comment *)"
  | Comma_tok -> print_string "comma ,"
  | Lbracket_tok -> print_string "lbracket ["
  | Rbracket_tok -> print_string "rbracket ]"
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
