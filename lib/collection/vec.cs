module collection.vec

import * from datatype.nat

type vec_t(A: type_t, length: nat_t) {
  null_vec_t(length: zero_t)
  cons_vec_t(n: nat_t, length: succ_t(n), head: A, tail: vec_t(A, n))
}

@infix(++)
vec_append(ante: vec_t(A, m), succ: vec_t(A, n)): vec_t(A, m + n) = {
  ante case {
    null_vec_t => succ
    cons_vec_t => cons_vec_t(
      head = ante.head,
      tail = ante.tail ++ succ)
  }
}

vec_map(f: A -> B, vec: vec_t(A, n)): vec_t(A, n) = {
  vec case {
    null_vec_t => vec
    cons_vec_t => cons_vec_t(
      head = f(vec.head),
      tail = vec_map(f, vec.tail))
  }
}
