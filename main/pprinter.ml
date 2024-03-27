open PPrint
open Lang


(* Global constant *)
let indent_level = 4

(* Constants printing *)
let doc_of_const = function
| BoolV true -> string "True"
| BoolV false -> string "False"
| IntV i -> PPrint.OCaml.int i
| FloatV f -> PPrint.OCaml.float f


(* Variables printing *)
let doc_of_var v = string v


(* Variable declarations printing *)
let doc_of_var_list vs = parens (separate_map comma string vs)


(* Binary operators printing *)
let doc_of_barith = function
| BAadd -> string "+"
| BAsub -> string "-"
| BAmul -> string "*"
| BAdiv -> string "/"
| BAmod -> string "%"

let doc_of_bcompar = function
| BCeq -> string "=="
| BCge -> string ">="
| BCgt -> string ">"
| BCle -> string "<="
| BClt -> string "<"
| BCne -> string "!="

let doc_of_blogic = function
| BLand -> string "and"
| BLor -> string "or"

let doc_of_binop = function
| BArith ba -> doc_of_barith ba
| BCompar bc -> doc_of_bcompar bc
| BLogic bl -> doc_of_blogic bl


(* Expressions printing *)
let rec doc_of_expr = function
| Const c -> doc_of_const c
| VarE v -> doc_of_var v
| BinOp (bop, e1, e2) -> parens (doc_of_expr e1 ^^ space ^^ doc_of_binop bop ^^ space ^^ doc_of_expr e2)
| IfThenElse(cond, t, e) -> parens (doc_of_expr t ^^ space ^^ string "if" ^^ space ^^ doc_of_expr cond ^^ space ^^ string "else" ^^ space ^^ doc_of_expr e)
| CallE(VarE(fn)::es) -> doc_of_var fn ^^ doc_of_expr_list es
| CallE(_) -> empty and doc_of_expr_list es = parens (separate_map comma doc_of_expr es)


(* Commands printing *)
let rec doc_of_cmd = function
| Skip -> empty
| Assign(vs, es) -> doc_of_var_list vs ^^ equals ^^ doc_of_expr_list es
| Seq(c1, c2) -> doc_of_cmd c1 ^^ doc_of_cmd c2
| Cond(e, c1, c2) -> string "if" ^^ space ^^ doc_of_expr e ^^ colon ^^
                     (nest indent_level (hardline ^^ (doc_of_cmd c1))) ^^
                     (match c2 with
                     | Skip -> empty
                     | _ -> hardline ^^ string "else" ^^ colon ^^
                            (nest indent_level (hardline ^^ (doc_of_cmd c2))))
| While(e, c) -> string "while" ^^ space ^^ doc_of_expr e ^^ colon ^^
                 (nest indent_level (hardline ^^ (doc_of_cmd c)))
| Return(e) -> (separate space [string "return"; doc_of_expr e])


(* Function and procedure definitions printing *)
let doc_of_fpdefn  = function
| Fundefn(FPdecl(rt, fn, params), e)->
      (string "def " ^^ string fn ^^ doc_of_var_list (List.map name_of_vardecl params) ^^ colon) ^^
      (nest indent_level (hardline ^^ (string "return"  ^^ doc_of_expr e))) ^^
      hardline
| Procdefn(FPdecl(rt, fn, params), c) ->
      (string "def " ^^ string fn ^^ doc_of_var_list (List.map name_of_vardecl params) ^^ colon) ^^
      (nest indent_level (hardline ^^ (doc_of_cmd c))) ^^
      hardline


(* Program printing *)
let doc_of_prog (Prog(fdfs, e)) = (separate_map hardline doc_of_fpdefn fdfs) ^^ hardline ^^
(string "print" ^^ (parens (doc_of_expr e))) ^^ hardline


(* Printing to channel *)
let print_prog prg =
  ToChannel.pretty 0.5 80 stdout (doc_of_prog prg);
  flush stdout

