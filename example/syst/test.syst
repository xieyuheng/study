let three: nat_t = succ(succ(succ(zero)))

let add: (nat_t, nat_t) -> nat_t = {
  (n, k) =>
    nat_rec(nat_t, n, k,
      (prev, almost) =>
        succ(almost))
}

eval! add(three)
eval! add(three, three)
