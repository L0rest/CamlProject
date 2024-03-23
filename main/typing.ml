open Lang

(* Exceptions *)
exception TypeError of string
exception TooManyArguments of string


(* Environments *)
 type environment = (vname * tp) list


(* Variable typing *)
let tp_var (env: environment) (valName: vname) : tp =
    try List.assoc valName env
    with Not_found -> failwith "error: variable not found"


(* Application typing *)
let rec tp_application (ft: tp) (ats: tp list) : tp = match ats with
    | [] -> ft
    | at :: atrest -> (match ft with
                    | FunT(fta, ftb) -> if fta = at then tp_application ftb atrest else raise (TypeError "Incorrect type")
                    | _ -> raise (TypeError "Not a function type"))


(* Expression typing *)
let rec tp_expr (env: environment) (exp: expr) : tp = match exp with
| Const (BoolV b) -> BoolT
| Const (IntV i) -> IntT
| Const (FloatV f) -> FloatT
| VarE v -> tp_var env v
| BinOp ((BArith op), e1, e2) -> let t1 = tp_expr env e1 in
                                 let t2 = tp_expr env e2 in
                                 if t1 = t2 && t1 = IntT then t1 else raise (TypeError "Arithmetic operation on different types")
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
                    tp_application ft at
| _ -> raise (TypeError "Not implemented")


(* Function definition typing *)
let tp_fdefn (env: environment) fundec = match fundec with
| Procdefn _ -> raise (TypeError "Procdefn unexpected")
| Fundefn (FPdecl (returnTp, fn, args), e) -> let add_var (Vardecl (v, t)) = (v, t) in
                                              let env' = List.map add_var args @ env in
                                              tp_expr env' e


(* Function type constructor *)
let construct_funtype (vds: vardecl list) (returnTp : tp) =
    let rec construct_funtype_aux vds = match vds with
        | (Vardecl(_,t) :: q) -> FunT(t, construct_funtype_aux q)
        | _ -> returnTp in
    construct_funtype_aux vds


(* Function used to fill env *)
let fillEnv = function
    | Fundefn(FPdecl(returnTp, fn, vardecls), _) -> (fn, construct_funtype vardecls returnTp)
    | _ -> raise (TypeError "Procdefn unexpected")


(* Program typing *)
let tp_prog (Prog (fdfs, e)) =
    let env = List.map fillEnv fdfs in
    let _ = List.map (tp_fdefn env) fdfs in
    tp_expr env e (* if the function ends without raising an exception, the program is well-typed *)