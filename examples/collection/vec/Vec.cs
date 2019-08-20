module collection.vec

union Vec {
  A: Type
  length: Nat
} unions {
  class NullVec {
    A: Type
    length: Nat

    length: Zero
  }

  class ConsVec {
    A: Type
    length: Nat

    n: Nat

    length: Succ(n)
    head: A
    tail: Vec(A, n)
  }
}

Vec {
  A: Type
  length: Nat
} {
  NullVec {
    A: Type
    length: Nat

    length: Zero
  }

  ConsVec {
    A: Type
    length: Nat

    n: Nat

    length: Succ(n)
    head: A
    tail: Vec(A, n)
  }
}

type Vec(A: Type, length: Nat) {
  NullVec(length: Zero)
  ConsVec(n: Nat, length: Succ(n), head: A, tail: Vec(A, n))
}

@infix(++)
vec_append(ante: Vec(A, m), succ: Vec(A, n)): Vec(A, m + n) =
  ante case {
    NullVec => succ
    ConsVec => ConsVec(
      head = ante.head,
      tail = ante.tail ++ succ)
  }

vec_map(fun: A -> B, vec: Vec(A, n)): Vec(A, n) =
  vec case {
    NullVec => vec
    ConsVec => ConsVec(
      head = fun(vec.head),
      tail = vec_map(fun, vec.tail))
  }
