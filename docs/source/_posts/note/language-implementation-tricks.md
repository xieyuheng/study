# Language implementation tricks

## Closure and lexical scope

- we need lexical scope for lambda

- the closure trick says:
  closure = lambda expression + env

- this is the most basic trick for writing
  simple call-by-value eval function

- whenever lexical scope is need,
  the trick of closure must be used.

## Keep the model of expression and value simple

- the expression is already simple

- value is also simple if
  the only difference between exp and value is the env closure

- the keep it simple trick says: keep it simple

## Implementation of inference rules

- type system is specified by logic inference rules

- such specification make it possible to prove e: T
  but might not provide algorithm for checking e: T
  i.e. might not generating a proof of e: T for you

  why not?
  just because we are not providing enough information in the syntax.

  for example,
  curry's typed lambda calculus has less information in syntax
  compare to church's typed lambda calculus.

- the bidirectional trick says:
  we can turn a set of inference rules to a type checking algorithm,
  by sometimes view a rule as infer sometimes as check.

  for example, the rule of function application:

  ``` js
  ctx |- f: A -> B
  ctx |- x: A
  ------------
  ctx |- f(x): B
  ```

  we need infer to check function application,
  and if we have infer, we can also infer the type of function application.
  thus for rule of function application we use infer.

  ``` js
  [infer] ctx |- f: A -> B
  [check] ctx |- x: A
  ------------
  [infer] ctx |- f(x): B
  ```

  we examine our rules recursively and do infer as far as possible.
  and for those left we do check.

  but the rule of (curry's) function abstraction:

  ``` js
  ctx, x: A |- e: B
  ------------
  ctx |- (x) => e: A -> B
  ```

  we can not possibly infer the type of a bound variable x.
  thus we do check for the rule of function abstraction.

  ``` js
  [check] ctx, x: A |- e: B
  ------------
  [check] ctx |- (x) => e: A -> B
  ```

  function abstraction is the constructor of function type,
  function application is the eliminator of function type,
  and since the rules about function are core of type systems,
  we propagate the property "we need to check instead of infer"
  to all rules about constructor.

  why we have this propagation ?

- why we sometimes need infer, when we only want to implementation check ?
  because we need to infer the type of target of elimination rules.

  for elimination rules the pattern is

  ``` js
  [infer] premise about target
  [check] premise
  [check] premise
  [check] ...
  ----------
  [infer] conclusion
  ```

  for construction rules the pattern is

  ``` js
  [check] premise
  [check] premise
  [check] ...
  ----------
  [check] conclusion
  ```

- can we just provide enough information in the syntax.
  for example, use church's typed lambda calculus instead of curry's typed lambda calculus ?
  in church's typed lambda calculus, can we infer all the way down ?
  is this true for all the rules about constructor ?

  ``` js
  [infer] ctx, x: A |- e: B
  ------------
  [infer] ctx |- (x: A) => e: A -> B
  ```

  we do not need to annotate the return type of function

- a sub-trick is that,
  the argument type of type check function
  should be (e: Exp, T: Val)
  instead of (e: Exp, T: Exp)

  the dependent version of the rule of function application

  ``` js
  ctx |- f: A -> B
  ctx |- x: A
  val_eq(val_apply(B, x), T)
  // B is a value that can apply to x
  //   thus it must be value (with closure)
  //   to maintain lexical scope
  ------------
  ctx |- f(x): T

  [infer] ctx |- f: A -> B
  [check] ctx |- x: A
  [assert] val_eq(val_apply(B, x), T)
  // B is a value that can apply to x
  //   thus it must be value (with closure)
  //   to maintain lexical scope
  ------------
  [infer] ctx |- f(x): T
  ```

- note about the duality (or variance) between
  premise and conclusion in inference rule

  ``` js 
  premise
  ----------
  conclusion
  ```

  is like function of type premise -> conclusion

  ``` js 
  [check] premise
  ----------
  [infer] conclusion
  ```

  can be read as,
  if we can implement check for premise,
  we can implement infer for conclusion.

  if we only need to implement check for premise to implement infer for conclusion,
  the rule will be useful in more places,
  for it is usable even if we can not implement infer premise.

## Comparing equivalence between expressions

- we can comparing equivalence if we can normalize

- the normalization by evaluation (aka. nbe, or norm-by-eval) trick says:
  eval the expressions to values,
  can read them back to normal form.

  because there will be undefined free variables,
  during the evaluation of nbe,
  we need neutral form for each eliminator to handle this,
  because eliminator might be applied to variable.

- we can also make comparing equivalence faster,
  by nbe the two expressions together,
  and know that they are not equal as soon as
  they start to appear to be not equal.

  comparing weak head normal form step by step.
