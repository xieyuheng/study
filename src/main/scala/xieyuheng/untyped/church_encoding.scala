package xieyuheng.untyped

import scala.annotation.tailrec
import xieyuheng.untyped._

object church_encoding {
  var m = Module()

  m.define("church_zero",
    Lambda("f", Lambda("x", Var("x"))))

  m.define("church_add1",
    Lambda("prev", Lambda("f", Lambda("x",
      Ap(Var("f"),
        Ap(Ap(Var("prev"), Var("f")),
          Var("x")))))))

  m.define("church_add",
    Lambda("j", Lambda("k", Lambda("f", Lambda("x",
      Ap(Ap(Var("j"), Var("f")),
        Ap(Ap(Var("k"), Var("f")),
          Var("x"))))))))

  def from_int(n: Int): Exp = {
    if (n <= 0) {
      Var("church_zero")
    } else {
      Ap(Var("church_add1"), from_int(n - 1))
    }
  }
}
