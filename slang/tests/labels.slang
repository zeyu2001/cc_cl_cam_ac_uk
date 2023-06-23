let L0 (x : int) : int =
  if x = 0 then 1 else x * L0 (x-1)
in
  L0 1
