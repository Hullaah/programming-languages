fun factorial (n) =
let
  fun tail_fact (n, fact) =
    if n = 0
    then fact
    else tail_fact(n-1, fact * n)
in
  tail_fact(n, 1)
end

