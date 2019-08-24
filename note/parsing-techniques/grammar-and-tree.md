# Grammar and Tree

I show two formats in this chapter,
they are little languages of parsing-tech :

- `.tree` -- to describe parse-tree
  - with a simple type system

- `.grammar` -- to describe corresponding context-free-grammar
  - in which each rule is uniquely named

## sexp

``` grammar
sexp:list = { "(" sexp-list ")" }
sexp:bool = { bool }

sexp-list:unit = { sexp }
sexp-list:cons = { sexp " " sexp-list }

bool:true = { "true" }
bool:false = { "false" }
```

## tom-dick-and-harry

``` grammar
tom-dick-and-harry:name = { name }
tom-dick-and-harry:list = { list " and " name }

name:tom = { "tom" }
name:dick = { "dick" }
name:harry = { "harry" }

list:unit = { name }
list:cons = { name ", " list }
```

``` tree
// > "tom, dick, and harry"
tom-dick-and-harry:list {
    list:cons {
        name:tom {"tom"}
        ", "
        list:unit {name:dick {"dick"}}}
    " and "
    name:harry {"harry"}}
```

## tdh

- regular grammar

``` grammar
tdh:t = { "t" }
tdh:d = { "d" }
tdh:h = { "h" }
tdh:list = { list }

list:t = { "t" list-tail }
list:d = { "d" list-tail }
list:h = { "h" list-tail }

list-tail:list = { "," list }
list-tail:t = { "&t" }
list-tail:d = { "&d" }
list-tail:h = { "&h" }
```

- left regular grammar

``` grammar
tdh-left:t = { "t" }
tdh-left:d = { "d" }
tdh-left:h = { "h" }
tdh-left:list = { list }

list:t = { list-head "&t" }
list:d = { list-head "&d" }
list:h = { list-head "&h" }

list-head:t = { "t" }
list-head:d = { "d" }
list-head:h = { "h" }
list-head:before-t = { list-head ",t" }
list-head:before-d = { list-head ",d" }
list-head:before-h = { list-head ",h" }
```

## sum

``` grammar
sum:digit = { digit }
sum:sum = { sum " + " sum }

digit:0 = { "0" }
...
digit:9 = { "9" }
```

``` tree
// > "3 + 5 + 1" -- ((3 + 5) + 1)
sum:sum {
    sum:sum {
        sum:digit {
            digit:3 {"3"}}
        " + "
        sum:digit {
            digit:5 {"5"}}}
    " + "
    sum:digit {
        digit:1 {"1"}}}
```

## sentence

``` grammar
sentence:svo = { subject verb object }

subject:the = { "the " noun }
subject:a = { "a " noun }
subject:name = { proper-name }

object:the = { "the " noun }
object:a = { "a " noun }
object:name = { proper-name }

verb:bit = { "bit" }
verb:chased = { "chased" }

noun:cat = { "cat" }
noun:dog = { "dog" }

proper-name:... = ...
```

## ab

- equal number of "a" "b"

``` grammar
ab:head-a = { "a" b }
ab:head-b = { "b" a }

a:one = { "a" }
a:more = { "a" s }
a:after-b = { "b" a a }

b:one = { "b" }
b:more = { "b" s }
b:after-a = { "a" b b }
```

``` tree
// > "aabb"
ab:head-a {
    "a"
    b:after-a {
        "a"
        b:one {"b"}
        b:one {"b"}}}
```

## abc

- ambiguous grammar
- language = "a"^m "b"^n "c"^n | "a"^p "b"^p "c"^q

``` grammar
abc:a-bc = { a bc }
abc:ab-c = { ab c }

a:one = { "a" }
a:more = { "a" a }

bc:one = { "bc" }
bc:more = { "b" bc "c" }

ab:one = { "ab" }
ab:more = { "a" ab "b" }

c:one = { "c" }
c:more = { "c" c }
```

``` tree
// > "aabb"
s:a-bc {
     a:more {"a" a:one {"a"}}
     bc:one {"bc"}}
```

## list

``` grammar
list : -- Gr -> Gr
list:null (t) = {}
list:cons (t) = { t list (t) }
```
- word-list

``` grammar
word:bye = { "bye" }
word:hello = { "hello" }

word-list = { list (word) }
bye-word-list = { list (word:bye) }
```

## non-empty-list

``` grammar
non-empty-list : -- Gr -> Gr
non-empty-list:unit (t) = { t }
non-empty-list:cons (t) = { t non-empty-list (t) }
```

## tom-dick-and-harry -- generic

``` grammar
one-or-more : -- Gr -> Gr
one-or-more:one (t) = { t }
one-or-more:more (t) = { t ", " one-or-more }

more-and-one : -- Gr -> Gr
more-and-one:one (t) = { t }
more-and-one:more (t) = { one-or-more (t) " and " t }

name:tom = { "tom" }
name:dick = { "dick" }
name:harry = { "harry" }

tom-dick-and-harry = { more-and-one (name) }
tom-and-tom = { one-or-more (name:tom) }
tom-and-harry = { one-or-more ([name:tom name:harry]) }
```

``` tree
// > "tom, dick, and harry"
more-and-one:more (name) {
    one-or-more:more (name) {
        name:tom {"tom"}
        ", "
        one-or-more:one (name) {name:dick {"dick"}}}
    " and "
    name:harry {"harry"}}
```

## higher order

``` grammar
line-map :
    -- -- t -> Gr t
    -> Gr
line-map:one (f t) = { f (t) }
line-map:more (f t) = { f (t) "\n" line-map (f t) }

tom-dick-and-harry-lines = { line-map (more-and-one name) }
```
