module datatype

type nat_t {
  zero_t
  succ_t(prev: nat_t)
}

nat_add(x: nat_t, y: nat_t): nat_t = {
  x case {
    zero_t => y
    succ_t => succ_t(nat_add(x.prev, y))
  }
}

nat_mul(x: nat_t, y: nat_t): nat_t = {
  x case {
    zero_t => zero_t
    succ_t => nat_add(y, nat_mul(x.prev, y))
  }
}

nat_factorial(x: nat_t): nat_t = {
  x case {
    zero_t => succ_t(zero_t)
    succ_t => nat_mul(x, nat_factorial(x.prev))
  }
}

nat_even_p(x: nat_t): bool_t = {
  x case {
    zero_t => true_t
    succ_t => x case {
      zero_t => false_t
      succ_t => nat_even_p(x.prev.prev)
    }
  }
}
