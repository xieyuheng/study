package xieyuheng.tartlet

import scala.annotation.tailrec

object Util {
  @tailrec
  def freshen(
    usedNames: Set[String],
    name: String,
  ): String = {
    if (usedNames.contains(name)) {
      freshen(usedNames, name + "*")
    } else {
      name
    }
  }

  def conversionCheck(
    ctx: Ctx,
    t: Value,
    v1: Value,
    v2: Value,
  ): Either[ErrorMsg, Unit] = {
    for {
      e1 <- v1.readBack (ctx, t)
      e2 <- v2.readBack (ctx, t)
    } yield if (e1.alphaEq (e2, Map(), Map())) {
      ()
    } else {
      ErrorMsg("conversionCheck fail")
    }
  }
}
