union list_t {
  null_t
  cons_t
} {
  t: type
}

class null_t {
  t: type
}

class cons_t {
  t: type
  car: t
  cdr: list_t (t)
}

list_length: (
  [implicit]: { t: type }
  list: list_t (t)
) -> nat_t = case (list) {
  null_t => zero_t ()
  cons_t => succ_t (list_length (list.cdr))
}

list_append: (
  [implicit]: { t: type }
  ante: list_t (t),
  succ: list_t (t),
) -> list_t (t) = case (ante) {
  null_t => succ
  cons_t => cons_t (ante.car, list_append (ante.cdr, succ))
}

list_map: (
  [implicit]: { a: type, b: type }
  fun: (a) -> b,
  list: list_t (a),
) -> list_t (b) = case (list) {
  null_t => list
  cons_t => cons_t (fun (list.car), list_map (fun, list.cdr))
}

list_remove_first: (
  [implicit]: { t: type }
  x: t,
  list: list_t (t),
) -> list_t (t) = case (list) {
  null_t => list
  cons_t => case (eq_p (list.car, x)) {
    true_t => list.cdr
    false_t => cons_t (list.car, list_remove_first (list.cdr, x))
  }
}

union list_length_t {
  zero_length_t
  succ_length_t
} {
  [implicit]: { t: type }
  list : list_t (t)
  length : nat_t
}

class zero_length_t {
  list: list_t (t) = null_t ()
  length: nat_t = zero_t ()
}

class succ_length_t {
  list: list_t (t) = cons_t (x, l)
  length: nat_t = succ_t (n)
  prev: list_length_t (l, n)
}

/** in prolog, we will have:
 * append([], Succ, Succ).
 * append([Car | Cdr], Succ, [Car | ResultCdr]):-
 *   append(Cdr, Succ, ResultCdr).
 */

union list_append_t {
  zero_append_t
  succ_append_t
} {
  [implicit]: { t: type }
  ante: list_t (t)
  succ: list_t (t)
  result: list_t (t)
}

class zero_append_t {
  [implicit]: { t: type }
  ante: list_t (t) = null_t ()
  succ: list_t (t)
  result: list_t (t) = succ
}

class succ_append_t {
  [implicit]: {
    t: type
    car: t
  }
  ante: cons_t (t, car)
  succ: list_t (t)
  result: cons_t (t, car)
  prev: list_append_t (
    ante.cdr,
    succ,
    result.cdr,
  )
}
