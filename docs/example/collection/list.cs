module collection.list

type list_t(A: type_t) {
  null_t
  cons_t(head: A, tail: list_t(A))
}
