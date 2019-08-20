module collection.list

union List {
  A: Type
} unions {
  class Null {
    A: Type
  }

  class Cons {
    A: Type
    head: A
    tail: List(A)
  }
}

type List(A: Type) {
  Null
  Cons(head: A, tail: List(A))
}
