# record type
- back to eopl module and class
- telescope of automath
- one of the most important techniques we need to learn
  is to handle scope of record-like structure
  such as module, object and class.
- [eopl.lang_module]
  what is the different between first class module and record type?
- [eopl.lang_class]
- [eopl] note about further reading
# inductive type
- we need two implement languages (one for cicada, one for jojo),
  with arbitrary inductive type definition
  and arbitrary recursive function.
  because we need such languages to do experiments.
- we do not care about consistency for now
- we also allow type in type
# jojo
- the cut level is stable
  only [x : A] and (- A)
  exe level jo in eval is also simple
  but exe level jo in cut can generate different pattern of cut level jo
- how to handle inductive type in jojo ?
- [adventure] both `(let x)` and `[x]`
- [adventure] syntax for explicit tail call
- [adventure.simple] `type <name> = { ... }`
- [adventure.simple] representing data types
- main adventure
  the algebraic structure is simple
  the hardest part is to implement `eqv` between `jojo_t`
  - [adventure.counted] annotation for arg_count and ret_count
  - [adventure.counted] infer arg_count and ret_count
  - [adventure.counted] eq by eval
  - [adventure.counted] encode permutation group
