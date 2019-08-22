package xieyuheng.tartlet

case class Cdr (
  pair: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] = {
    for {
      pairValue <- pair.eval(env)
      res <- Cdr.exe(pairValue)
    } yield res
  }

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case Cdr(pair2) =>
        pair.alphaEq(pair2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx: p => Sigma(x: A, D)
   ----------------
   ctx :- Cdr(p) => D subst (x, Car(p))
   */
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    for {
      the <- pair.infer(ctx)
      value <- the.t.eval(ctx.toEnv)
      res <- value match {
        case ValueSigma(carType, cdrType) =>
          for {
            pairValue <- the.value.eval(ctx.toEnv)
            carValue <- Car.exe(pairValue)
            realCdrType <- cdrType.apply(carValue)
            cdrTypeExp <- realCdrType.readBack(ctx, ValueUniverse)
          } yield The(cdrTypeExp, the.value)
        case _ =>
          Left(ErrorMsg(
            s"expected the type to be Sigma(carType, cdrType), found: ${the.t}"))
      }
    } yield res
  }
}

object Cdr {
  def exe(
    pair: Value,
  ): Either[ErrorMsg, Value] = {
    pair match {
      case ValueCons(car, cdr) =>
        Right(cdr)
      case TheNeutral(ValueSigma(carType, cdrType), neutral) => {
        for {
          carValue <- Car.exe(pair)
          realCdrType <- cdrType.apply(carValue)
        } yield TheNeutral(realCdrType, NeutralCar(neutral))
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
