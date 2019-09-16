package xieyuheng.tartlet

import readback._

object conversion_check {
  def apply(ctx: Ctx, t: Val, v1: Val, v2: Val): Either[Err, Unit] =
    conversion_check(ctx: Ctx, t: Val, v1: Val, v2: Val)

  def conversion_check(ctx: Ctx, t: Val, v1: Val, v2: Val): Either[Err, Unit] = {
    for {
      e1 <- readback_val(v1, t, ctx)
      e2 <- readback_val(v2, t, ctx)
    } yield if (alpha_eq(e1, e2, Map(), Map())) {
      ()
    } else {
      Err("conversion_check fail")
    }
  }
}
