let rec pgcd (a : int) (b : int) : int =
  if a = b then a
  else if a > b then pgcd (a - b) b
  else pgcd a (b - a)
;;

pgcd 15 5;;