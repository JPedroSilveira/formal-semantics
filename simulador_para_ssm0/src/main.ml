type expr =
  ExTrue
| ExFalse
| ExIf     of expr * expr * expr
| ExZero 
| ExSucc   of expr
| ExPred   of expr
| ExIsZero of expr
    
type instr =
  PUSH of int 
| POP
| COPY
| INC
| DEC
| JMP of int
| JMPZR of int 
    
type ssm0 = instr list 
  
type stack = int list
  
type state = ssm0 * stack
           
exception EndOfProg 
exception InvalidArg 
exception JmpOutOfProg
exception EmptyStack

let rec drop n list =
if (n <0) then raise InvalidArg else
  match (n,list) with
    (0,_) ->  Some list
  | (n, _::t) -> drop (n-1) t
  | (n, []) -> None 
    
let step: state -> state = function
  ([], stack) -> raise EndOfProg
| ((PUSH z) :: code, stack) -> (code, z :: stack)
| ((JMP n)  :: code, stack) -> 
    (match drop n code with
       Some (code') -> (code', stack)
     | None -> raise JmpOutOfProg)
| (POP :: code, _ :: stack) -> (code, stack)
| (COPY :: code, z :: stack) -> (code, z :: z ::stack) 
| (INC :: code, z :: stack) -> (code, (z + 1) ::stack) 
| (DEC :: code, z :: stack) -> (code, (z - 1) ::stack)
| ((JMPZR n) :: code, 0::stack) -> 
    (match drop n code with
       Some (code') -> (code', stack)
     | None -> raise JmpOutOfProg)
| ((JMPZR n) :: code, z::stack) -> (code, stack) 
| (POP :: code, []) -> raise EmptyStack  
| (COPY ::code, []) -> raise EmptyStack 
| (INC :: code, []) -> raise EmptyStack
| (DEC :: code, []) -> raise EmptyStack
| ((JMPZR n) :: code, []) ->  raise EmptyStack 

let rec eval c =
try 
  let c'= step c 
  in eval c' 
with JmpOutOfProg -> ("Jump fora do programa.", c)
   | EndOfProg    -> ("Fim do programa.", c)
   | EmptyStack   -> ("Operação com pilha vazia.", c)
   | InvalidArg    -> ("Desvio com argumento negativo.", c) 
                      
let rec comp (e:expr) : ssm0 = 
match e with 
  ExTrue -> [PUSH 1]
| ExFalse -> [PUSH 0]
| ExZero -> [PUSH 0]
| ExSucc e1 -> (comp e1) @ [INC]
| ExPred e1 -> (comp e1) @ [COPY; JMPZR 1; DEC]
| ExIsZero e1 -> (comp e1) @ [JMPZR 2; PUSH 0; JMP 1; PUSH 1]
| ExIf(e1,e2,e3) -> 
    let c2 = comp e2 in
    let c3 = comp e3 in
    let n2 = List.length c2 in
    let n3 = List.length c3 in
    (comp e1) @ [JMPZR (n2 + 1)] @ c2 @ [JMP n3] @ c3

let c1: ssm0 = [PUSH 1; INC; INC; POP; POP]
let c2: ssm0 = [JMP 3; PUSH 1; INC]
let c3: ssm0 = [JMP (-1); PUSH 1; INC]
             
let s1: stack = [2; 3] 
let st: state = (c1, s1)
              
let e1 = ExIsZero(ExZero)
let e2 = ExZero
let e3 = ExSucc(ExZero)
let eif = ExIf(e1,e2,e3)
let e4 = ExIsZero(ExSucc(ExFalse))
let e5 = ExIsZero(ExFalse)
let e6 = ExIf(e1, ExZero, ExSucc ExZero)