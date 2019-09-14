package xieyuheng.tartlet

case class Car (
  pair: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] = {
    for {
      pairVal <- pair.eval(env)
      res <- Car.exe(pairVal)
    } yield res
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Car(pair2) =>
        pair.alphaEq(pair2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx: p => Sigma(x: A, D)
   ----------------
   ctx :- Car(p) => A
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      the <- pair.infer(ctx)
      res <- the.t match {
        case Sigma(name, carType, cdrType) =>
          for {
            carTypeVal <- carType.eval(ctx.toEnv)
            carTypeExp <- carTypeVal.readback(ctx, ValUniverse)
          } yield The(carTypeExp, the.value)
        case _ =>
          Left(Err(
            s"expected the type to be Sigma(carType, cdrType), found: ${the.t}"))
      }
    } yield res
  }
}

object Car {
  def exe(
    pair: Val,
  ): Either[Err, Val] = {
    pair match {
      case ValCons(car, cdr) =>
        Right(car)
      case TheNeu(ValSigma(carType, cdrType), neutral) => {
        Right(TheNeu(carType, NeuCar(neutral)))
      }
      case _ =>
        Left(Err(
          "pair should be " +
            "ValCons(car, cdr) | " +
            "TheNeu(ValSigma(carType, cdrType), neutral): " +
            s"${pair}"))
    }
  }
}
