let rec prod (a : int) (b : int) (acc : int) : int =
  if b = 0 then acc
  else prod a (b - 1) (acc + a)
;;

prod 3 5 0 ;;