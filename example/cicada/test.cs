list_t : (type) -> type
list_t = (A) => datatype {
  null : datacons {}
  cons : (A, list_t(A)) -> datacons {}
}

list_append : (type, list_t(A), list_t(A)) -> list_t(A)
list_append = (A, null, y) => y
list_append = (A, cons(head, tail), y) =>
  cons(head, list_append(A, tail, y))

and_t : (type, type) -> type
and_t = (P, Q) => (R: type) -> ((P, Q) -> R) -> R

conj : (P: type, Q: type, P, Q) -> and_t(P, Q)
conj = (P, Q, x, y) => (z, f) => f(x, y)

vec_t : (type, nat_t) -> type
vec_t = (A) => datatype {
  vec_null : datacons { n == zero }
  vec_cons : (A, prev: nat_t, vec_t(A, prev)) -> datacons {
    n == succ[prev]
  }
}

//////

list_t : (type) -> type
list_t = [A] datatype {
  null : datacons {}
  cons : (A, list_t(A)) -> datacons {}
}

list_append : (type, list_t(A), list_t(A)) -> list_t(A)
list_append = [A, null, y] y
list_append = [A, cons(head, tail), y]
  cons(head, list_append(A, tail, y))

and_t : (type, type) -> type
and_t = [P, Q] (R: type) -> ((P, Q) -> R) -> R

conj : (P: type, Q: type, P, Q) -> and_t(P, Q)
conj = [P, Q, x, y] [z, f] f(x, y)

vec_t : (type, nat_t) -> type
vec_t = [A] datatype {
  vec_null : datacons { n == zero }
  vec_cons : (A, prev: nat_t, vec_t(A, prev)) -> datacons {
    n == succ[prev]
  }
}
