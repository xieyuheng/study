# Constructive Mathematics and Computer Programming

------
- Author: Martin-Löf
- Date: 1979
------

# intro

- the whole conceptual apparatus of programming
  mirrors that of modern mathematics
  (set theory, that is, not geometry)
  and yet is supposed to be different from it.
  How come?
  The reason for this curious situation is, I think,
  that the mathematical notions have
  gradually received an interpretation,
  the interpretation which we refer to as classical,
  which makes them unusable for programming.

- it is clear that

  if a function is defined as a binary relation
  satisfying the usual existence and unicity conditions,
  whereby classical reasoning is allowed in the existence proof,
  or a set of ordered pairs
  satisfying the corresponding conditions,
  then a function cannot be the same kind of thing
  as a computer program.

  similarly,
  if a set is understood in Zermelo’s way
  as a member of the cumulative hierarchy,
  then a set cannot be the same kind of thing as a data type.

# expressions

- The expressions of the theory of types
  are formed out of variables,
  by means of various forms of expression.

- a expression can be evaluated to get a value.

- I shall call an expression, in whatever notation,
  canonical [or normal]
  if it is already fully evaluated,
  which is the same as to say that
  it has itself as value.
  thus, evaluation is idempotent.

- x -
  the reason that
  - we can not only have value,
    we also need to have expression.
  is because we uses variables.
  variables are not value.

- x -
  the notion of canonical and non-canonical expressions,
  is to capture
  the notion of data and program form [code].

- x -
  the author uses lazy-eval,
  and outside-first eval order.

  while in sequent1,
  I used eager-eval,
  and postfix notation.

  the notion of 'not-yet-determined object'
  made lazy-eval not necessary.

- x -
  to define the theory of type,
  we must specify
  what canonical and non-canonical expressions
  we already have.

- table of the primitive forms of expression :

  | canonical type     |               | non-canonical      |
  |--------------------|---------------|--------------------|
  | type               | intro         | elim               |
  |--------------------|---------------|--------------------|
  | (Pi (: :x A) B)    | (Lambda :x B) | (c a)              |
  | (Sigma (: :x A) B) | (* a b)       | (E [:x :y] c d)    |
  | (+ A B)            | (i a) (j b)   | (D [:x :y] c d e)  |
  | (I A a b)          | refl          | (J c d)            |
  | N0                 |               | (R0 c)             |
  | N1 0_1             |               | (R1 c c0)          |
  | N2 0_2 1_2         |               | (R2 c c0 c1)       |
  | ...                |               | ...                |
  | N 0 a'             |               | (R [:x :y] c d e)  |
  | (W (: :x A) B)     | (sup a b)     | (T [:x :y :z] c d) |
  | Universe0          |               |                    |
  | Universe1          |               |                    |
  | ...                |               |                    |

# judgements

- four forms of judgements in type theory :
  1. A is a type -- (: A Type)
  2. A and B are equal types -- (= A B)
  3. a is an object of type A -- (: a A)
  4. a and b are equal objects of type A -- (= a b : A)

- three forms of judgements in predicate logic :
  [whether classical or intuitionistic]
  1. A is a formula.
  2. A is true.
  3. a is an individual term.

- A canonical type A is defined by prescribing :
  1. how a canonical object of type A is formed.
  2. how two equal canonical objects of type A are formed.

  For noncanonical A,
  a judgment of the form (: A Type)
  means A has a canonical type as value.

  There is no limitation on this prescription
  except that the relation of equality
  which it defines between canonical objects of type A
  must be reflexive, symmetricand transitive.

- Bishop -
  A set is not an entity which has an ideal existence.
  A set exists only when it has been defined.
  To define a set we prescribe, at least implicitly,
  1. what we (the constructing intelligence) must do
     in order to construct an element of the set,
  2. and what we must do to show that
     two elements of the set are equal.

- If the rules for forming canonical objects
  as well as equal canonical objects of a certain type
  are called the introduction rules for that type,
  we may thus say with Gentzen that
  a canonical type (proposition)
  is defined by its introduction rules.

- Two canonical types A and B are equal
  if a canonical object of type A
  is also a canonical object of type B
  and, moreover, equal canonical objects of type A
  are also equal canonical objects of type B,
  and vice versa.

  - x -
    very strong [hard to prove] property.

  For arbitrary types A and B,
  [not necessarily canonical]
  a judgment of the form (= A B) means that
  A and B have equal canonical types as values.

- x -
  about implementation of type and equality.
  1. we use induction to define type,
     which provides us data-constructors
     to construct elements of the type.
  2. we use structural equality as basic equality,
     we can derive from this basic equality by quotient
     to form quotient-type.

- x -
  by the definition of type
  we must be able to implement predicates for judgments :
  (3) a is an object of type A -- (: a A)
  (4) a and b are equal objects of type A -- (= a b : A)

  - note how the use of not-yet-determined objects
    will impact the semantics of (3) and (4)

  - and not how unification is different from equality.

# >< inference

- natural deduction :
  ><><><

- x -
  can we make variable substitution better in the rules ?

- x -
  in sequent1
  inference rule should also be expressed by arrow type.
