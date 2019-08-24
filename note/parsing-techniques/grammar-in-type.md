# Grammar in Type

To embed formal-grammar in rust type definition
- by rust proc_macro
- for native algebraic datatype generation
  - i.e. serde support for linear-parse-tree format

## sexp

``` rust
grammar! {
    Sexp::List = "(" SexpList ")";
    Sexp::Bool = Bool;

    SexpList::Unit = Sexp;
    SexpList::Cons = Sexp " " SexpList;

    Bool::True = "true";
    Bool::False = "false";
}

grammar_in_type! {
    enum Sexp {
        List ("(", Box <SexpList>, ")"),
        Bool (Bool),
    }

    enum SexpList {
        Null (""),
        Cons (Sexp, " ", Box <SexpList>),
    }

    enum Bool {
        True ("true"),
        False ("false"),
    }
}

// > "(true (false) true)
// Sexp::List (Box::new (
//     SexpList::Cons (
//         Sexp::Bool (Bool::True),
//         SexpList::Cons (
//             Sexp::List (Box::new (
//                 SexpList::Cons (
//                     Sexp::Bool (Bool::False),
//                     SexpList::Null))),
//             SexpList::Cons (
//                 Sexp::Bool (Bool::True),
//                 SexpList::Null)))))
```

## ab

``` rust
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

grammar_in_type! {
    enum S {
        HeadA ("a", B),
        HeadB ("b", A),
    }

    enum A {
        One ("a"),
        More ("a", S),
        AfterB ("b", A, A),
    }

    enum B {
        One ("b"),
        More ("b", S),
        AfterA ("a", B, B),
    }
}

// > "aabb"
// S::HeadA (B::AfterA (B::One (), B::One ()))
```

## abc

``` rust
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

grammar_in_type! {
    enum S {
        A_BC (A, BC),
        AB_C (AB, C),
    }

    enum A {
        One ("a"),
        More ("a", Box <A>),
    }

    enum BC {
        One ("bc"),
        More ("b", Box <BC>, "c"),
    }

    enum AB {
        One ("ab"),
        More ("a", Box <AB>, "b"),
    }

    enum C {
        One ("c"),
        More ("c", Box <C>),
    }
}

//  "aabc"
// S::A_BC (A::More (Box::new (A::One)), BC::One)
```
