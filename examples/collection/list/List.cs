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
