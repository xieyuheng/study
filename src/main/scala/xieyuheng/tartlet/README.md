## References

- "Checking Dependent Types with Normalization by Evaluation: A Tutorial"
  - by David Thrane Christiansen
  - origin tutorial at :: http://davidchristiansen.dk/tutorials/nbe
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
  Lambda("n", Lambda("k",
    IndNat(
      Var("n"),
      Lambda("_", Nat),
      Var("k"),
      Lambda("prev", Lambda("almost",
        Add1(Var("almost"))))))))

m.run(Var("three"))
// The(Nat,Add1(Add1(Add1(Zero))))

m.run(Apply(Var("+"), Var("three")))
// The(Pi(_*,Nat,Nat),Lambda(_*,Add1(Add1(Add1(Var(_*))))))

m.run(Apply(Apply(Var("+"), Var("three")), Var("three")))
// The(Nat,Add1(Add1(Add1(Add1(Add1(Add1(Zero)))))))
```
