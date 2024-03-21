open Lang

(* Exceptions *)
exception TypeError of string
exception TooManyArguments of string

(* Environments *)
type environment =
    {localvar: (vname * tp) list;
     funbind: (vname * fpdecl) list}


(* Variable typing *)
let tp_var env valName =
try List.assoc valName env.localvar
with Not_found -> let FPdecl (t, _, _) = List.assoc valName env.funbind in t


(* Statement typing *)
let rec tp_stmt (ft: tp) (ats: tp list) = match ats with
    | (at :: atrest) -> match ft with
                        | FunT(fta, ftb) -> if fta = at then tp_stmt ftb atrest else raise (TypeError "Wrong type for argument")
                        | _ -> raise (TooManyArguments "Too many arguments")
    | _ -> ft


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
                    tp_stmt ft at
| _ -> raise (TypeError "Not implemented")


(* Function used to fill env.localvar *)
let fillLocalVar = function
    | Vardecl(v,t) -> (v, t)
    | _ -> raise (TypeError "Not a parameter")


(* Function used to fill env.funbind *)
let fillFunBind = function
    | Fundefn(fpdec, _) -> (name_of_fpdecl fpdec, fpdec)
    | _ -> raise (TypeError "Procdefn unexpected")


(* Function definition typing *)
let tp_fdefn env (Fundefn (dec, e)) = let env = {localvar = env.localvar @ List.map fillLocalVar (params_of_fpdecl dec); (* On ajoute les parametres de la fonction a l'environnement local *)
                                                funbind = env.funbind} in
                                      if tp_expr env e != (tp_of_fpdecl dec) then raise (TypeError "Function definition")
                                      else env


(* Program typing *)
let tp_prog (Prog (fdfs, e)) =
    let env = {localvar = []; funbind = List.map fillFunBind fdfs} in
    let env = List.fold_left tp_fdefn env fdfs in
    tp_expr env e