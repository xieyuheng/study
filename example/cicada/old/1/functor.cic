class functor_t {
  fun_t: type -> type

  map: (
    [implicit]: { a: type, b: type }
    f: a -> b
    c: fun_t (a)
  ) -> fun_t (b)
}

list_functor = functor_t (
  fun_t = list_t

  map: (
    [implicit]: { a: type, b: type }
    f: a -> b
    list: list_t (a)
  ) -> list_t (b) = {
    list case {
      null_t => null_t
      cons_t => cons_t (f (list.car), map (f, list.cdr))
    }
  }
)
