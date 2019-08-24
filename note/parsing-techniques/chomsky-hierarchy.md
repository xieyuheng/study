# Chomsky Hierarchy

In context-free-grammar,
I can use non-terminal-symbol and named-choice
to uniquely identify each grammar rule.

I will use this identification to form parse-trees.

The macro `grammar!` means "context_free_grammar".

## sexp

``` rust
phrase_grammar! {
    Sexp = Bool | "(" SexpList ")";
    SexpList = Sexp | Sexp " " SexpList;
    Bool = "true" | "false";
}

grammar! {
    Sexp::List = "(" SexpList ")";
    Sexp::Bool = Bool;

    SexpList::Unit = Sexp;
    SexpList::Cons = Sexp " " SexpList;

    Bool::True = "true";
    Bool::False = "false";
}
```

## ab

``` rust
phrase_grammar! {
    S = "a" B | "b" A;
    A = "a" | "a" S | "b" A A;
    B = "b" | "b" S | "a" B B;
}

grammar! {
    S::HeadA = "a" B;
    S::HeadB = "b" A;

    A::One = "a";
    A::More = "a" S;
    A::AfterB = "b" A A;


    B::One = ("b");
    B::More = "b" S;
    B::AfterA = "a" B B;
}
```

## abc

``` rust
phrase_grammar! {
    S = A BC | AB C;
    A = "a" | "a" A;
    BC = "bc" | "b" BC "c";
    AB = "ab" | "a" AB "b";
    C = "c" | "c" C;
}

grammar! {
    S::A_BC = A BC;
    S::AB_C = AB C;

    A::One = "a";
    A::More = "a" A;

    BC::One = "bc";
    BC::More = "b" BC "c";

    AB::One = "ab";
    AB::More = "a" AB "b";

    C::One = "c";
    C::More = "c" C;
}
```
