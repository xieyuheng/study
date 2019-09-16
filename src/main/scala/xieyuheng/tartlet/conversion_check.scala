package xieyuheng.tartlet

object conversion_check {
  def apply(ctx: Ctx, t: Val, v1: Val, v2: Val): Either[Err, Unit] =
    conversion_check(ctx: Ctx, t: Val, v1: Val, v2: Val)

  def conversion_check(ctx: Ctx, t: Val, v1: Val, v2: Val): Either[Err, Unit] = {
    for {
      e1 <- v1.readback_val(ctx, t)
      e2 <- v2.readback_val(ctx, t)
    } yield if (alpha_eq(e1, e2, Map(), Map())) {
      ()
    } else {
      Err("conversion_check fail")
    }
  }
}
