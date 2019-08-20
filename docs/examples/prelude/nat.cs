module prelude

union Nat {} unions {
  class Zero {}
  class Succ {
    prev: Nat
  }
}

add(x: Nat, y: Nat): Nat =
  x case {
    Zero => y
    Succ => Succ(add(x.prev, y))
  }

mul(x: Nat, y: Nat): Nat =
  x case {
    Zero => Zero
    Succ => add(y, mul(x.prev, y))
  }

factorial(x: Nat): Nat =
  x case {
    Zero => Succ(Zero)
    Succ => mul(x, factorial(x.prev))
  }

even_p(x: Nat): Bool =
  x case {
    Zero => True
    Succ => x case {
      Zero => False
      Succ => even_p(x.prev.prev)
    }
  }
