module collection.list

union List {
  A: type_t
} unions {
  class Null {
    A: type_t
  }

  class Cons {
    A: type_t
    head: A
    tail: List(A)
  }
}

type List(A: type_t) {
  Null
  Cons(head: A, tail: List(A))
}
