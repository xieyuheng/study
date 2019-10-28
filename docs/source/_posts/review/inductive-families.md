# Inductive Families

------
- Author: Peter Dybjer
- Date: 1994
------

# 1 Introduction

When extending Martin-Löf's type theory,
instead of using a general purpose construction which is part of the theory,
we giving external criteria for correct extensions of the theory.

The main point here is that *Martin-Löf's type theory
is a theory of inductive definitions formulated in natural deduction*.
Each set former (logical constant) is defined inductively by its introduction rules.
The elimination rule expresses a principle of definition by recursion (proof by induction).
Equality rules express how these definitions are eliminated (proofs are normalised).

First we specify what it means to be a correct definition of a set former
by giving formal criteria for the formation and introduction rules.
Then we show how such a definition determines the elimination and equality rules
by a so called *inversion principle*. We also give an alternative formulation where recursive
definitions are presented schematically.

The scheme is for *monomorphic, intensional type theory*
and covers *strictly positive, iterated, generalised induction*.
This does not cover all forms of intuitionistically meaningful induction,
and thus not all ways of forming sets in Martin-Löf's type theory.
(An example is the definition of a universe in Tarski's style.)

# 2 The Theory of Logical Types

TODO

# 3 A Scheme for Inductive Definitions
