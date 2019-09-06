module collection.list

type list_t(A: type_t) {
  null_t
  cons_t(head: A, tail: list_t(A))
}

list_length(list: list_t): nat_t = {
  list case {
    null_t => zero_t
    cons_t => succ_t(list_length(list.tail))
  }
}

list_append(A: type_t, ante: list_t(A), succ: list_t(A)): list_t(A) = {
  ante case {
    null_t => succ
    cons_t => cons_t(ante.head, list_append(ante.tail, succ))
  }
}

list_map(A: type_t, B: type_t, f: A -> B, list: list_t(A)): list_t(B) = {
  list case {
    null_t => list
    cons_t => cons_t(fun(list.head), list_map(fun, list.tail))
  }
}
