open Lang

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     funbind: (vname * fpdecl) list}


(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = let rec getFpdecl = function
                               (Fundefn(dec,_) :: q) -> let FPdecl(_, n, _) = dec in (n, dec) :: getFpdecl q
                               | _ -> [] in
{localvar = []; funbind = getFpdecl fdfs};;


(* Recherche d'une variable dans l'environnement *)

exception NotFound

(* Typage de variables *)
let tp_var env valName = let rec searchVal x = function
                         ((k,v) :: q) -> if k = x then v else searchVal x q
                         | [] -> raise NotFound in
try searchVal valName env.localvar with NotFound -> let FPdecl (t, _, _) = searchVal valName env.funbind in t;;

