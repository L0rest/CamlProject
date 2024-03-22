open Lang

(* Exceptions *)
exception TypeError of string
exception TooManyArguments of string

(* Environments *)
 type environment = (vname * tp) list

(* Variable typing *)
let tp_var (env: environment) valName =
    try List.assoc valName env
    with Not_found -> failwith "error: variable not found"


(* Statement typing *)
let rec tp_stmt ft at = match ft,at with
| (FunT(in_t,out_t), (a :: q)) -> if in_t = a then tp_stmt out_t q else raise (TypeError "Bad argument type")
| (FunT(in_t,out_t), []) -> ft
| _ -> if at = [] then ft else raise (TooManyArguments "Too many arguments")


(* A definir.
On souhaite calculer le type d'une application f a1 a2 ... an.
On sait que
f  a le type ft et les a1 :: [a2, ... an] ont les types at :: atrest.
On peut remarquer que si f: fta -> ftb (voir le match en bas), alors il faut avoir fta = at
(a faire en bas),
et il faut recursivement verifier que pour (f a1) : ftb, les types de [a2, ... an] : atrest sont corrects
    *)
let rec tp_application (ft: tp) (ats: tp list) :  tp = match ats with
    | [] -> ft
    | at :: atrest ->   (* A definir *)
        (match ft with
        | FunT(fta, ftb) -> if fta = at then tp_application ftb atrest else raise (TypeError "Incorrect type")
        | _ -> failwith "incorrect type"
        )

(* Expression typing *)
let rec tp_expr env exp = match exp with
| Const (BoolV b) -> BoolT
| Const (IntV i) -> IntT
 (* N' est pas defini
    | Const (FloatV f) -> FloatT *)
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
let tp_fdefn env (Fundefn (FPdecl(tdec,ndec,args), e)) =
let add_var (Vardecl (v, t)) = (v, t) in
let env' = List.map add_var args @ env in
tp_expr env' e



(* A definir. Le resultat n'est pas le bon.
Pour une fonction comme let f(a1:A1 ... an:An) : Res = body,
construire le type A1 -> A2 ... -> An -> Res
    *)
let construct_funtype fn vds returnTp =
    let rec construct_funtype_aux vds = match vds with
        | [] -> returnTp
        | (Vardecl(_,t) :: q) -> FunT(t, construct_funtype_aux q) in
    construct_funtype_aux vds

(* Function used to fill env.funbind *)
let fillFunBind = function
    | Fundefn(FPdecl(returnTp, fn, vardecls), _body) -> (fn, construct_funtype fn vardecls returnTp)
    | _ -> raise (TypeError "Procdefn unexpected")

(* Program typing *)
(* Reste a faire; la verification de types de chaque definition de fonction *)
let tp_prog (Prog (fdfs, e)) =
    let env = List.map fillFunBind fdfs in
    let _ = List.map (tp_fdefn env) fdfs in
        tp_expr env e (* if the function ends without raising an exception,
                                                the program is well-typed *)
