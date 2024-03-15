open Lang

(* Exceptions *)
exception TypeError of string

(* Environments *)
type environment =
    {localvar: (vname * tp) list;
     funbind: (vname * fpdecl) list}


(* Variable typing *)
let tp_var env valName =
try List.assoc valName env.localvar
with Not_found -> let FPdecl (t, _, _) = List.assoc valName env.funbind in t


(* Statement typing *)
let rec tp_stmt (FunT(in_t, out_t)) at = match at with
| (a :: q) -> if a = in_t then tp_stmt out_t q else raise (TypeError "CallE on non-function")
| _ -> FunT(in_t, out_t)


(* Expression typing *)
let rec tp_expr env exp = match exp with
| Const (BoolV b) -> BoolT
| Const (IntV i) -> IntT
| Const (FloatV f) -> FloatT
| VarE v -> tp_var env v
| BinOp ((BArith op), e1, e2) -> let t1 = tp_expr env e1 in
                                 let t2 = tp_expr env e2 in
                                 if t1 = t2 && t1 = IntT || t1 = FloatT then t1 else raise (TypeError "Arithmetic operation on different types")
| BinOp ((BCompar op), e1, e2) -> let t1 = tp_expr env e1 in
                                  let t2 = tp_expr env e2 in
                                  if t1 = t2 then BoolT else raise (TypeError "Comparison operation on different types")
| BinOp ((BLogic op), e1, e2) -> let t1 = tp_expr env e1 in
                                 let t2 = tp_expr env e2 in
                                 if t1 = t2 && t1 = BoolT then BoolT else raise (TypeError "Logical operation on different types")
| IfThenElse (e1,e2,e3) -> let t1 = tp_expr env e1 in
                           let t2 = tp_expr env e2 in
                           let t3 = tp_expr env e3 in
                           if t1 = BoolT && t2 = t3 then t2 else raise (TypeError "IfThenElse")
| CallE(f::args) -> let ft = tp_expr env f in
                    let at = List.map (tp_expr env) args in
                    if not(List.mem ft [IntT; FloatT; BoolT]) then tp_stmt ft at
                    else raise (TypeError "CallE on non-function")
| _ -> raise (TypeError "Not implemented")


(* Program typing *)
let tp_prog (Prog (fdfs, e)) = let rec getFpdecl = function
                               | (Fundefn(dec,_) :: q) -> (name_of_fpdecl dec, dec) :: getFpdecl q
                               | ((Procdefn _) :: q) -> raise (TypeError "Procdefn unexpected")
                               | _ -> [] in
                               tp_expr {localvar = []; funbind = getFpdecl fdfs} e


(* Function definition typing *)
let tp_fdefn env (Fundefn (dec, e)) = let FPdecl(_,_,param) = dec in
                                      let rec getLocalVar = function
                                      | (Vardecl(v,t) :: q) -> (v, t) :: getLocalVar q
                                      | _ -> [] in
                                      tp_expr {localvar = getLocalVar param; funbind = env.funbind} e