# Simply Typed JoJo Calculus

------
- Author: Xie Yuheng
- Date: 2019-10-14
- Keywords: type system.
------

## Abstract

### Contains

## Introduction

## A Review of De Bruijn notation

De Bruijn notation is a useful translation of lambda expression,
after which the name of a variable binding is placed close to the argument it binds.

- **[example]** TODO

TODO The effect of this translation postfix.

## Adding function composition into De Bruijn notation

TODO When postfix notation occur we can use stack machine to give semantics.
(We learned this from the programming language Forth)

- **[claim]** When translating lambda expressions to De Bruijn notation, function composition does not occur.
- **[proof]** TODO

## The algebraic structure of the space of simple type

- **[claim]** The algebraic structure of the space of simple type is freely generated group with quotation.
- **[demonstration]** We can claim this, because we break the arrow type `A -> B` into two `(- A)` and `B`.
  By "quotation", I mean `{A}`.
**[example]** TODO
- **[note]** we can also say,
  by "simple" we means the space is freely generated,
  which means there are no equations between types,
  such as conversion relations in the case of lambda expressions.

## Appendixes

### A Review of Lambda Calculus

## References
