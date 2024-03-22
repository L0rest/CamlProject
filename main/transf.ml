(* Transformation of Caml code to Python code *)

open Lang

module StringSet = Set.Make(String)


(* TODO: implement *)
let rec names_expr e = StringSet.empty

let rec is_tailrec_expr fname e = match e with
| VarE v -> v != fname
| BinOp(_, e1, e2) -> is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| IfThenElse(cond, e1, e2) -> is_tailrec_expr fname cond && is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| CallE (f::args) -> List.for_all (is_tailrec_expr fname) args
| _ -> true

let rec transf_expr fname paraml e = match e with
| IfThenElse(cond, e1, e2) ->
      if is_tailrec_expr fname cond then
          IfThenElse (transf_expr fname paraml cond, transf_expr fname paraml e1, transf_expr fname paraml e2)
      else e
(*need to patch *)
|CallE l -> if List.exists(fun x -> not (is_tailrec_expr fname x)) l then e
else CallE (List.map (transf_expr fname paraml) l)
| _ -> e

(* TODO: implement *)
let transf_fpdefn (FPdecl(tp, fname, params), e) = (FPdecl(tp, fname, params), e)

(* Not sure *)
let transf_prog (Prog(fdfs, e)) = IntT

