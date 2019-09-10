package xieyuheng.tartlet

case class ValueCons (car: Value, cdr: Value) extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    for {
      car <- car.readback(ctx, t)
      cdr <- cdr.readback(ctx, t)
    } yield Cons(car, cdr)
}
