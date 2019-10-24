# Simply Typed JoJo Calculus

------
- Author: Xie Yuheng
- Date: 2019-10-14
- Keywords: Type system.
------

## Abstract

### Contains

## Introduction

## A Review of Lambda Calculus

We assume the reader has a basic familiarity with the lambda calculus
and the concepts of bound variables, alpha-, beta- and eta-reduction.

We give a review of lambda calculus in this section,
to let the reader be familiar with our notations.

Since lambda application is not associative,
we use non-symmetrical syntax to avoid ambiguity.

- **[Definition]** lambda expression
  - variable: `x`
  - lambda application: `f(x)`
  - lambda abstraction: `(x) => body`

## A Review of De Bruijn notation

De Bruijn notation is a useful translation of lambda expression,
after which the name of a variable binding is placed near to the argument it binds.

- **[Example]** TODO

TODO The effect of this translation postfix.

## Adding function composition into De Bruijn notation

TODO When postfix notation occur we can use stack machine to give semantics.
(We learned this from the programming language Forth)

- **[Claim]** When translating lambda expressions to De Bruijn notation, function composition does not occur.
- **[Proof]** TODO

## The algebraic structure of the space of simple type

- **[Claim]** The algebraic structure of the space of simple type is freely generated group with quotation.
- **[Demonstration]** We can claim this, because we break the arrow type `A -> B` into two `(- A)` and `B`.
  By "quotation", I mean `{ A }`.
- **[Example]** TODO
- **[Note]** We can also say,
  by "simple" we means the space is freely generated,
  which means there are no equations between types,
  such as conversion relations in the case of lambda expressions.

## Appendixes

## References
