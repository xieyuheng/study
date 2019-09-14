package xieyuheng.tartlet

case class ValCons (car: Val, cdr: Val) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    for {
      car <- car.readback(ctx, t)
      cdr <- cdr.readback(ctx, t)
    } yield Cons(car, cdr)
}
