package xieyuheng.tartlet

case class ValueCons (car: Value, cdr: Value) extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    for {
      car <- car.readBack(ctx, t)
      cdr <- cdr.readBack(ctx, t)
    } yield Cons(car, cdr)
}
