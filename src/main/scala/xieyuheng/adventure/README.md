# problem

## untyped

- implicit `exe` is confusing

- explicit `exe` will make
  local function definition different from
  global function definition

- when using `let` in definition of type and function,
  the order of `let` are matched.
  but when using function,
  the args must be supplied in reversed order

## dependent

- when defining neutral to use norm-by-eval
  - need infer `arg_count` and `ret_count`
  - need projection to handle multi return value
