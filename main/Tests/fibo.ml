let rec fibo (n : int) : int =
  if n <= 0 then 0
  else if n = 1 then 1
  else fibo (n - 1) + fibo (n - 2)
;;

fibo 10;;