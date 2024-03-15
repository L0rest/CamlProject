(* Transformation of Caml code to Python code *)

open Lang


module StringSet = Set.Make(String)

(* TODO: implement *)
let rec names_expr = StringSet.empty


let rec is_tailrec_expr fname e = match e with
| VarE v -> v != fname
| BinOp(_, e1, e2) -> is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| IfThenElse(cond, e1, e2) -> is_tailrec_expr fname cond && is_tailrec_expr fname e1 && is_tailrec_expr fname e2
| CallE l -> let rec is_tailrec_call = function
             | (a :: q) -> is_tailrec_expr fname a && is_tailrec_call q
             | _ -> true
             in is_tailrec_call l
| _ -> true

let rec transf_expr fname paraml e = match e with
| IfThenElse(cond, e1, e2) ->
      if is_tailrec_expr fname cond then
          IfThenElse (transf_expr fname paraml cond, transf_expr fname paraml e1, transf_expr fname paraml e2)
      else e
(* Not working *)
|CallE l -> if List.exists(fun x -> not (is_tailrec_expr fname x)) l then e else Assign(paraml, l)
| _ -> Return e

(* TODO: implement *)
let transf_fpdefn (FPdecl(tp, fname, params), e) =


let transf_prog (Prog(fdfs, e)) = let new_fdfs = List.map transf_fpdefn fdfs in Prog(new_fdfs, e)
