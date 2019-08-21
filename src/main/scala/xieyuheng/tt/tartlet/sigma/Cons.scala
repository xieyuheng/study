package xieyuheng.tt.tartlet

case class Cons (car: Exp, cdr: Exp) extends Constructor {
  def eval(env: Env): Either[ErrorMsg, Value] =
    for {
      car <- car.eval(env)
      cdr <- cdr.eval(env)
    } yield ValueCons(car, cdr)

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Cons(car2, cdr2) =>
        car.alphaEq(car2, thisMap, thatMap) &&
        cdr.alphaEq(cdr2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- car <= A
   ctx.ext(x, A) :- cdr <= D
   -----------------
   ctx :- Cons(car, cdr) <= Sigma(x: A, D)
   */
  def check(ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    t match {
      case ValueSigma(carType, cdrType) =>
        for {
          car <- car.check(ctx, carType)
          carValue <- car.eval(ctx.toEnv)
          realCdrType <- cdrType.apply(carValue)
          cdr <- cdr.check(ctx, realCdrType)
        } yield Cons(car, cdr)
      case _ =>
        Left(ErrorMsg(
          s"expected ValueSigma(carType, cdrType), found: ${t}"))
    }
}
