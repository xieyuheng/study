package xieyuheng.tartlet

import scala.annotation.tailrec

object util {
  @tailrec
  def freshen(
    used_names: Set[String],
    name: String,
  ): String = {
    if (used_names.contains(name)) {
      freshen(used_names, name + "*")
    } else {
      name
    }
  }

  def conversion_check(
    ctx: Ctx,
    t: Val,
    v1: Val,
    v2: Val,
  ): Either[Err, Unit] = {
    for {
      e1 <- v1.readback_val(ctx, t)
      e2 <- v2.readback_val(ctx, t)
    } yield if (e1.alpha_eq (e2, Map(), Map())) {
      ()
    } else {
      Err("conversion_check fail")
    }
  }
}
