package xieyuheng.tartlet

case class Car (
  pair: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      pairValue <- pair.eval(env)
      res <- Car.exe(pairValue)
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
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    for {
      the <- pair.infer(ctx)
      res <- the.t match {
        case Sigma(name, carType, cdrType) =>
          for {
            carTypeValue <- carType.eval(ctx.toEnv)
            carTypeExp <- carTypeValue.readBack(ctx, ValueUniverse)
          } yield The(carTypeExp, the.value)
        case _ =>
          Left(ErrorMsg(
            s"expected the type to be Sigma(carType, cdrType), found: ${the.t}"))
      }
    } yield res
  }
}

object Car {
  def exe(
    pair: Value,
  ): Either[ErrorMsg, Value] = {
    pair match {
      case ValueCons(car, cdr) =>
        Right(car)
      case TheNeutral(ValueSigma(carType, cdrType), neutral) => {
        Right(TheNeutral(carType, NeutralCar(neutral)))
      }
      case _ =>
        Left(ErrorMsg(
          "pair should be " +
            "ValueCons(car, cdr) | " +
            "TheNeutral(ValueSigma(carType, cdrType), neutral): " +
            s"${pair}"))
    }
  }
}
