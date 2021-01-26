type expr = 
    ExTrue 
  | ExFalse
  | ExIf of expr * expr * expr
  | ExZero 
  | ExSucc of expr
  | ExPred of expr
  | ExIsZero of expr

exception NoRuleApplies

let rec isnumericval(e: expr): bool =
  match e with
    ExZero -> true
  | ExSucc(e1) -> isnumericval(e1)
  | _ -> false

let rec step (e: expr): expr = 
  match e with
    ExIf(ExTrue,e2, _) -> e2
  | ExIf(ExFalse,_,e3) -> e3
  | ExIf(e1,e2,e3) -> let e1' = step e1 in ExIf(e1',e2,e3)
  | ExSucc(e1) -> let e1' = step e1 in ExSucc(e1')
  | ExPred(ExZero) -> ExZero
  | ExPred(ExSucc(nv)) when isnumericval(nv) -> nv
  | ExPred(e1) -> let e1' = step e1 in ExPred(e1')
  | ExIsZero(ExZero) -> ExTrue
  | ExIsZero(ExSucc(nv1)) when isnumericval(nv1) -> ExFalse
  | ExIsZero(e1) -> let e1' = step e1 in ExIsZero(e1')
  | _ -> raise NoRuleApplies

let rec eval (e: expr): expr = 
  try let e' = step e
    in eval e'
  with NoRuleApplies -> e

let rec isval (e: expr): bool = 
  match e with
  ExTrue -> true
| ExFalse -> true
| ExZero -> true
| ExSucc(e1) when isnumericval(e1) -> true
| ExPred(e2) when isnumericval(e2) -> true
| _ -> false

let preresult (e: expr) =
  let result = eval e
  in if isval result then Printf.printf "sucesso" else Printf.printf "erro"

let e1 = ExIsZero(ExZero)
let e2 = ExZero
let e3 = ExSucc(ExZero)
let eif = ExIf(e1, e2, e3)
let e4 = ExIsZero(ExSucc(ExZero))
let e5 = ExIsZero(ExFalse)
let e6 = ExIf(e1, e5, e2)