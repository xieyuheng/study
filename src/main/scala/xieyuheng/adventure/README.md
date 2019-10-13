# adventure

## untyped

- explicit `exe` will make
  lookup of local definition (`let`) is different from
  lookup of global definition ( <name> = { ... } )

- when using `let` in definition of type and function,
  the order of `let` are matched.
  but when using function,
  the args must be supplied in reversed order
  - this is a feature for algebraicity

## simple

## dependent

- when defining neutral to use norm-by-eval
  - need infer `arg_count` and `ret_count`
  - need projection to handle multi return value

- [adventure.dependent]
- [adventure.dependent] infer arg_count and ret_count
- [adventure.dependent] equivalent_val
- [adventure.dependent] `assert_eq` for testing
- encoding like lambda calculus
- how to implement call-by-name jojo calculus ?
- how to implement call-by-need jojo calculus ?
- what are continuation and cps in jojo calculus ?
- how to encode lambda calculus by jojo calculus ?
- how to encode pure type system by jojo calculus ?
- for lambda calculus,
  we have a lattice of type systems.
  how to get a lattice of type systems for jojo ?
  use something like pure type system ?
- how to implement jojo calculus by rewriting ?
