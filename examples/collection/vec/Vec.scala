module collection.vec

union Vec {
  A: Type
  length: Nat
} unions {
  class NullVec {
    length: Zero
  }

  class ConsVec {
    // implicit: {
    //   n: Nat
    // }
    length: Succ(n)
    head: A
    tail: Vec(A, n)
  }
}

class ConsVec {
  // from union
  A: Type
  length: Nat
  // implicit
  n: Nat

  length: Succ(n)
  head: A
  tail: Vec(A, n)
}

RecordValue(
  name = "ConsVec",
  map = ListMap(
    "A" -> LogicVar("#A"),
    "length" -> UnionValue(name = "Nat"),
    "n" -> UnionValue(name = "Nat"),

    "length" -> ???,
    "head" -> LogicVar("#A"),
    "tail" -> UnionValue(name = "Nat")),
  bind = Map())

Bind (
  LogicVar("A") -> ,

)

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
