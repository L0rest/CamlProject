(* Transformation of Caml code to Python code *)

open Lang

module StringSet = Set.Make(String)


let rec names_expr  = function
    Const(_) -> StringSet.empty
   | VarE v -> StringSet.singleton v
   | BinOp(_, e1, e2) -> StringSet.union (names_expr e1) (names_expr e2)
   | IfThenElse(cond, e1, e2) -> StringSet.union (names_expr cond) (StringSet.union (names_expr e1) (names_expr e2))
   | CallE (f::args) -> List.fold_left (fun acc a -> StringSet.union acc (names_expr a)) (names_expr f) args
   | _ -> StringSet.empty


let rec is_tailrec_expr fname e = match e with
 VarE v -> v != fname
| BinOp(_, e1, e2) -> is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| IfThenElse(cond, e1, e2) -> not (StringSet.mem fname (names_expr cond)) && is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| CallE (f::args) -> List.for_all (fun  a -> not (StringSet.mem fname (names_expr a))) args
| e -> not (StringSet.mem fname (names_expr e))


let transf_expr fname paraml e = if is_tailrec_expr fname e then
let rec trad_expr = function
| IfThenElse(cond,e1,e2) -> Cond(cond, trad_expr e1, trad_expr e2)
| CallE (f::args) -> Assign(paraml, args)
| e -> Return e in
While(Const(BoolV true),(trad_expr e))
else Return e


let transf_fpdefn (Fundefn(FPdecl(ft,vname,decl),e)) = let nlist = List.map (fun (Vardecl(n,_)) -> n) decl in
Procdefn(FPdecl(ft,vname,decl), transf_expr vname nlist e)


let transf_prog (Prog(fundefns, e)) = Prog(List.map transf_fpdefn fundefns, e)
