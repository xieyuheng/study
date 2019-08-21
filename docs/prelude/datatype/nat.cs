module datatype

type nat_t {
  zero_t
  succ_t(prev: nat_t)
}

add(x: nat_t, y: nat_t): nat_t = {
  x case {
    zero_t => y
    succ_t => succ_t(add(x.prev, y))
  }
}

mul(x: nat_t, y: nat_t): nat_t = {
  x case {
    zero_t => zero_t
    succ_t => add(y, mul(x.prev, y))
  }
}

factorial(x: nat_t): nat_t = {
  x case {
    zero_t => succ_t(zero_t)
    succ_t => mul(x, factorial(x.prev))
  }
}

even_p(x: nat_t): bool_t = {
  x case {
    zero_t => true_t
    succ_t => x case {
      zero_t => false_t
      succ_t => even_p(x.prev.prev)
    }
  }
}
