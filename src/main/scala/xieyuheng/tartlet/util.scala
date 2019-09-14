package xieyuheng.tartlet

import scala.annotation.tailrec

object util {
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
    t: Val,
    v1: Val,
    v2: Val,
  ): Either[Err, Unit] = {
    for {
      e1 <- v1.readback (ctx, t)
      e2 <- v2.readback (ctx, t)
    } yield if (e1.alphaEq (e2, Map(), Map())) {
      ()
    } else {
      Err("conversionCheck fail")
    }
  }
}
