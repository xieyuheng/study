package xieyuheng.tartlet

case class ValCons (car: Val, cdr: Val) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    for {
      car <- car.readback_val(ctx, t)
      cdr <- cdr.readback_val(ctx, t)
    } yield Cons(car, cdr)
}
