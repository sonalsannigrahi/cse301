let rec fact = fun (x: int) ->
  if x = 0 then 1
  else x * fact (x - 1) in
fact 10
