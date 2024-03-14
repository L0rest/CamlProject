(* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)
let rec names_expr = StringSet.empty

(* TODO: implement *)
let transf_prog (Prog(fdfs, e)) = Prog(fdfs, e)


let rec is_tailrec_expr fname e = match e with
| VarE v -> v != fname
| BinOp(_, e1, e2) -> is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| IfThenElse(cond, e1, e2) -> is_tailrec_expr fname cond && is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| CallE l -> let rec is_tailrec_call = function
             | (a :: q) -> is_tailrec_expr fname a && is_tailrec_call q
             | _ -> true
             in is_tailrec_call l
| _ -> true