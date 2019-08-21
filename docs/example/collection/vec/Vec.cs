module collection.vec

type vec_t(A: type_t, length: Nat) {
  null_vec_t(length: Zero)
  cons_vec_t(n: Nat, length: Succ(n), head: A, tail: Vec(A, n))
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
