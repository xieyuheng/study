module collection.vec

union Vec {
  A: Type
  length: Nat
} unions {
  class NullVec {
    length: Zero()
  }

  class ConsVec {
    length: Succ(n)
    head: A
    tail: Vec(A, n)
  }
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
