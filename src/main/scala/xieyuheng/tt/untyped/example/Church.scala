package xieyuheng.tt.untyped.example

import scala.annotation.tailrec
import xieyuheng.tt.untyped._

object Church {
  var m = Module()

  m.define("church_zero",
    Lambda("f", Lambda("x", Var("x"))))

  m.define("church_add1",
    Lambda("prev", Lambda("f", Lambda("x",
      Apply(Var("f"),
        Apply(Apply(Var("prev"), Var("f")),
          Var("x")))))))

  m.define("church_add",
    Lambda("j", Lambda("k", Lambda("f", Lambda("x",
      Apply(Apply(Var("j"), Var("f")),
        Apply(Apply(Var("k"), Var("f")),
          Var("x"))))))))

  def fromInt(n: Int): Exp = {
    if (n <= 0) {
      Var("church_zero")
    } else {
      Apply(Var("church_add1"), fromInt(n - 1))
    }
  }
}
