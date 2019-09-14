package xieyuheng.tartlet

case class Cons (car: Exp, cdr: Exp) extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    for {
      car <- car.eval(env)
      cdr <- cdr.eval(env)
    } yield ValCons(car, cdr)

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
  def check(ctx: Ctx, t: Val): Either[Err, Exp] =
    t match {
      case ValSigma(carType, cdrType) =>
        for {
          car <- car.check(ctx, carType)
          carVal <- car.eval(ctx.toEnv)
          realCdrType <- cdrType.apply(carVal)
          cdr <- cdr.check(ctx, realCdrType)
        } yield Cons(car, cdr)
      case _ =>
        Left(Err(
          s"expected ValSigma(carType, cdrType), found: ${t}"))
    }
}
