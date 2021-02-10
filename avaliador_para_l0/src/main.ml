open Format

exception NoRuleApplies
exception TypeError
exception ErrorInTypeInfer

let print = print_string
let space = print_space

type expr = 
  |  ExTrue 
  | ExFalse
  | ExIf of expr * expr * expr
  | ExZero 
  | ExSucc of expr
  | ExPred of expr
  | ExIsZero of expr

type type_l0 = Nat | Bool

let rec type_infer (e: expr) : type_l0 =
  match e with
    | ExTrue -> Bool
    | ExFalse -> Bool
    | ExIf(e1,e2,e3) -> 
      (match (type_infer e1, type_infer e2, type_infer e3) with 
        (Bool, t2, t3) when t2 = t3 -> t2
        | _ -> raise TypeError)
    | ExZero -> Nat
    | ExSucc e1 -> 
      (match (type_infer e1) with
        Nat -> Nat
        | _ -> raise TypeError)
    | ExPred e1 ->
      (match (type_infer e1) with
        Nat -> Nat
        | _ -> raise TypeError)
    | ExIsZero e1 ->
      (match (type_infer e1) with
        Nat -> Bool
        | _ -> raise TypeError)

let rec is_numeric_val (e: expr): bool =
  match e with
  | ExZero -> true
  | ExSucc(e1) -> is_numeric_val(e1)
  | _ -> false

let rec small_step (e: expr): expr = 
  match e with
    | ExIf(ExTrue,e2, _) -> e2
    | ExIf(ExFalse,_,e3) -> e3
    | ExIf(e1,e2,e3) -> let e1' = small_step e1 in ExIf(e1',e2,e3)
    | ExSucc(e1) -> let e1' = small_step e1 in ExSucc(e1')
    | ExPred(ExZero) -> ExZero
    | ExPred(ExSucc(nv)) when is_numeric_val(nv) -> nv
    | ExPred(e1) -> let e1' = small_step e1 in ExPred(e1')
    | ExIsZero(ExZero) -> ExTrue
    | ExIsZero(ExSucc(nv1)) when is_numeric_val(nv1) -> ExFalse
    | ExIsZero(e1) -> let e1' = small_step e1 in ExIsZero(e1')
    | _ -> raise NoRuleApplies

let rec eval_small_step (e: expr): expr = 
  try let e' = small_step e
    in eval_small_step e'
  with NoRuleApplies -> e

let rec eval_big_step (e: expr): expr =
  match e with
    | ExTrue -> ExTrue
    | ExFalse -> ExFalse
    | ExIf(e1,e2,e3) -> 
      (match eval_big_step e1 with
        ExTrue -> eval_big_step(e2)
        | ExFalse -> eval_big_step(e3)
        | _ -> raise ErrorInTypeInfer)
    | ExZero -> ExZero
    | ExSucc e1 -> if is_numeric_val(e1) then ExSucc e1 else raise ErrorInTypeInfer
    | ExPred e1 -> 
      (match e1 with
       ExZero -> ExZero
       | ExSucc x when is_numeric_val(x) -> x
       | _ -> raise ErrorInTypeInfer)
    | ExIsZero e1 -> 
      (match e1 with
        ExZero -> ExTrue
        | ExSucc x when is_numeric_val(e1) -> ExFalse
        | _ -> raise ErrorInTypeInfer)

let rec pr_expr = function
  | ExTrue -> print "true"
  | ExFalse -> print "false"
  | ExZero -> print "0"
  | ExIf(e1,e2,e3) -> open_hovbox 1; print "if"; space();
    pr_expr e1; space(); print "then"; space();
    pr_expr e2; space(); print "else"; space();
    pr_expr e3; space(); close_box()
  | ExSucc e1 -> open_hovbox 1; print "succ("; pr_expr e1; print ")"; space(); close_box()
  | ExPred e1 -> open_hovbox 1; print "pred("; pr_expr e1; print ")"; space(); close_box()
  | ExIsZero e1 -> open_hovbox 1; print "iszero("; pr_expr e1; print ")"; space(); close_box()

let rec pr_ty = function
  | Nat -> print "nat"
  | Bool -> print "bool"

let infer (e: expr) =
  try
    let t = type_infer e in
    let v = eval_big_step e in 
     (pr_expr v; print " : "; pr_ty t)
  with TypeError -> print_endline("Type error")
    | ErrorInTypeInfer -> print_endline("Bug in type infer system")

let e1 = ExIsZero(ExZero)
let e2 = ExZero
let e3 = ExSucc(ExZero)
let eif = ExIf(e1, e2, e3)
let e4 = ExIsZero(ExSucc(ExZero))
let e5 = ExIsZero(ExFalse)
let e6 = ExIf(e1, e5, e2)