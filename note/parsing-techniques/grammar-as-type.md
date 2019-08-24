---
title: Grammar as Type
author: Xie Yuheng
date: 2018-10-04
---

# Introduction

The correspondence between formal grammar and type theory.

# Context Free Grammar

``` rust
grammar! {
  Name -> "tom" | "dick" | "harry";
  Sentence -> Name | List " and " Name;
  List -> Name | Name ", " List;
}
```

``` cicada
name-t = literal! [
  "tom"
  "dick"
  "harry"
]

sentence-t = + [
  name-t
  mutil-name-sentence-t
]

mutil-name-sentence-t = * [
  name-t
  literal! [ " and " ]
  name-list-t
]

name-list-t = + [
  name-t
  mutil-name-list-t
]

mutil-name-list-t = * [
  name-t
  literal! [ ", " ]
  name-list-t
]
```

# Monotonic Grammar

``` rust
grammar! {
  S -> "abc" | "a" S Q;
  "b" Q "c" -> "bbcc";
  "c" Q -> Q "c";
}
```

``` cicada
S-t = + [
  literal! [ "abc" ]
  SQ-t
]

SQ-t = + [
  literal! [ "a" ]
  S-t
  Q-t
]

Q-t = union [extend-Q-t, swap-Q-t] {
  ante : context-t
  succ : context-t
}

context-t = list-t (string-t)

extend-Q-t = data {
  ante = list! [ "b" ]
  succ = list! [ "c" ]
  rewrite-to : * [ literal! [ "bbcc" ] ]
}

swap-Q-t = data {
  ante = list! [ "c" ]
  succ = list! [ ]
  rewrite-to : * [ Q-t, literal! [ "c" ] ]
}
```

# Context Sensitive Grammar
