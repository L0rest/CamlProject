open Lang

(* Environments *)
type environment =
    {localvar: (vname * tp) list;
     funbind: (vname * fpdecl) list}


(* Program typing *)
let tp_prog (Prog (fdfs, e)) = let rec getFpdecl = function
                               | (dec :: q) -> (name_of_fpdecl n, dec) :: getFpdecl q
                               | _ -> [] in
{localvar = []; funbind = getFpdecl fdfs}


(* Expression typing *)
let rec tp_expr env exp = match exp with
| Const (BoolV b) -> BoolT
| Const (IntV i) -> IntT
| Const (FloatV f) -> FloatT
| VarE v -> tp_var env v
| Binop ((BArith op), e1, e2) -> let t1 = tp_expr env e1 in
                                 let t2 = tp_expr env e2 in
                                 if t1 = t2 && t1 = IntT || t1 = FloatT then t1 else raise (TypeError "Arithmetic operation on different types")
| Binop ((BCompar op), e1, e2) -> let t1 = tp_expr env e1 in
                                  let t2 = tp_expr env e2 in
                                  if t1 = t2 then BoolT else raise (TypeError "Comparison operation on different types")
| Binop ((BLogic op), e1, e2) -> let t1 = tp_expr env e1 in
                                 let t2 = tp_expr env e2 in
                                 if t1 = t2 && t1 = BoolT then BoolT else raise (TypeError "Logical operation on different types")
| IfThenElse (e1,e2,e3) -> let t1 = tp_expr env e1 in
                           let t2 = tp_expr env e2 in
                           let t3 = tp_expr env e3 in
                           if t1 = BoolV && t2 = t3 then t2 else raise (TypeError "IfThenElse")
| CallE l -> let rec getTypes = function
             | (x :: q) -> (tp_expr env x) :: getTypes q
             | _ -> [] in
             getTypes l


(* Variable typing *)
exception NotFound

let tp_var env valName = let rec searchVal x = function
                         | ((k,v) :: q) -> if k = x then v else searchVal x q
                         | [] -> raise NotFound in
try searchVal valName env.localvar with NotFound -> let FPdecl (t, _, _) = searchVal valName env.funbind in t
