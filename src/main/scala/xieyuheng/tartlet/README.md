## References

- "Checking Dependent Types with Normalization by Evaluation: A Tutorial"
  - by David Thrane Christiansen
  - the original tutorial at :: http://davidchristiansen.dk/tutorials/nbe
- "The little typer"
  - by Daniel P. Friedman and David Thrane Christiansen

## Examples

``` scala
import xieyuheng.tartlet._

var m = Module()

m.claim("three", Nat)
m.define("three", Add1(Add1(Add1(Zero))))

m.claim("+", Arrow(Nat, Arrow(Nat, Nat)))
m.define("+",
  Fn("n", Fn("k",
    NatInd(
      Var("n"),
      Fn("_", Nat),
      Var("k"),
      Fn("prev", Fn("almost",
        Add1(Var("almost"))))))))

m.run(Var("three"))
// The(Nat,Add1(Add1(Add1(Zero))))

m.run(Ap(Var("+"), Var("three")))
// The(Pi(_*,Nat,Nat),Fn(_*,Add1(Add1(Add1(Var(_*))))))

m.run(Ap(Ap(Var("+"), Var("three")), Var("three")))
// The(Nat,Add1(Add1(Add1(Add1(Add1(Add1(Zero)))))))
```
