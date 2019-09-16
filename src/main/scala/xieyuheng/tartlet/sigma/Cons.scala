package xieyuheng.tartlet

case class Cons (car: Exp, cdr: Exp) extends Constructor {
  def eval(env: Env): Either[Err, Val] =
    for {
      car <- car.eval(env)
      cdr <- cdr.eval(env)
    } yield ValCons(car, cdr)

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case Cons(car2, cdr2) =>
        car.alpha_eq(car2, this_map, that_map) &&
        cdr.alpha_eq(cdr2, this_map, that_map)
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
          carVal <- car.eval(ctx.to_env)
          realCdrType <- cdrType.apply(carVal)
          cdr <- cdr.check(ctx, realCdrType)
        } yield Cons(car, cdr)
      case _ =>
        Left(Err(
          s"expected ValSigma(carType, cdrType), found: ${t}"))
    }
}
