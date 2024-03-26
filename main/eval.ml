open Lang

(* ************************************************************ *)
(* ****  Operational Semantics                             **** *)
(* ************************************************************ *)

(* ****  Auxiliary functions                               **** *)

type result =
| Val of value
| Closure of fpdefn * (result list)

type state = (vname * result) list


let rec lookup (x: vname) (s: state) : result = match s with
    [] -> failwith ("Variable " ^ x ^ " not found")
  | (y, v) :: s' -> if x = y then v else lookup x s'

let rec update (x: vname) (v: result) (s: state) : state =
    match s with
        [] -> [(x, v)]
    | (y, _) :: s' -> if x = y then (x, v) :: s' else (y, lookup y s) :: update x v s'


let arithOperation (op: barith) v1 v2 : value = match op with
| BAadd -> IntV(v1 + v2)
| BAsub -> IntV(v1 - v2)
| BAmul -> IntV(v1 * v2)
| BAdiv -> IntV(v1 / v2)
| BAmod -> IntV(v1 mod v2)


let compOperation (op: bcompar) v1 v2 : value = match op with
| BCeq -> BoolV(v1 = v2)
| BCne -> BoolV(v1 <> v2)
| BClt -> BoolV(v1 < v2)
| BCle -> BoolV(v1 <= v2)
| BCgt -> BoolV(v1 > v2)
| BCge -> BoolV(v1 >= v2)


let logicOperation (op: blogic) v1 v2 : value = match op with
| BLand -> BoolV(v1 && v2)
| BLor -> BoolV(v1 || v2)


let rec eval_expr (e: expr) (s: state) : result = match e with
| Const(BoolV b) -> Val(BoolV b)
| Const(IntV i) -> Val(IntV i)
| Const(FloatV f) -> Val(FloatV f)
| VarE v -> lookup v s
| BinOp (op, e1, e2) -> (match op with
                          | (BArith op) -> let Val(IntV(v1)) = eval_expr e1 s in
                                           let Val(IntV(v2)) = eval_expr e2 s in
                                           Val(arithOperation op v1 v2)
                          | (BCompar op) -> let Val(BoolV(v1)) = eval_expr e1 s in
                                            let Val(BoolV(v2)) = eval_expr e2 s in
                                            Val(compOperation op v1 v2)
                          | (BLogic op) -> let Val(BoolV(v1)) = eval_expr e1 s in
                                           let Val(BoolV(v2)) = eval_expr e2 s in
                                           Val(logicOperation op v1 v2))
| IfThenElse (cond, e1, e2) -> let Val(BoolV(b)) = eval_expr cond s in
                               if b then eval_expr e1 s else eval_expr e2 s
| CallE (f::args) -> failwith "TODO"
| _ -> failwith "not implemented"


(* **** Result of evaluation **** *)

(* TODO: implement *)
let eval_prog (Prog (fdfs, e)) = eval_expr (BinOp(BArith(BAadd), Const(IntV 2), Const(IntV 3))) []