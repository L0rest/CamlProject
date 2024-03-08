
open Lang
  
(* Environments *)

type environment = 
    {localvar: (vname * tp) list; 
     funbind: (vname * fpdecl) list}


(* TODO: implement *)
let tp_prog (Prog (fdfs, e)) = IntT
