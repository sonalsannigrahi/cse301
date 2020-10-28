let rec f = fun (x:int) ->
  let rec g = fun (y:int) ->
    if y <= x then
      f (y)
    else
      f (-y)
  in
  if x <= 0 then
    5
  else
    g (f (x-3))
in
f 3
