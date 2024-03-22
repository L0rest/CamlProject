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

let rec transf_expr fname paraml e = match e with
    | CallE (VarE f'::args) when f' = fname ->
        let assignments = List.map2 (fun param arg -> Assign([param], [arg])) paraml args in
        let seq_assignments = List.fold_right (fun a b -> Seq(a, b)) assignments Skip in
        While (Const (BoolV true), seq_assignments)
    | IfThenElse(cond, e1, e2) ->
        Cond (cond, transf_expr fname paraml e1, transf_expr fname paraml e2)
    | _ -> Return e

let transf_fpdefn (Fundefn(dec,e)) = Fundefn(dec, transf_expr (name_of_fpdecl dec) (List.map name_of_vardecl (params_of_fpdecl dec)) e)

let transf_prog (Prog(fundefns, e)) = Prog(List.map transf_fpdefn fundefns, transf_expr )
