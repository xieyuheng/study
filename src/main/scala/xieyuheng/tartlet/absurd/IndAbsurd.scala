package xieyuheng..tartlet

case class IndAbsurd (
  target: Exp,
  motive: Exp,
) extends Eliminator {
  def eval(env: Env): Either[ErrorMsg, Value] =
    for {
      targetValue <- target.eval(env)
      motiveValue <- motive.eval(env)
      res <- IndAbsurd.exe(targetValue, motiveValue)
    } yield res

  def alphaEq(
    that: Exp,
    thisMap: Map[String, String],
    thatMap: Map[String, String],
  ): Boolean = {
    that match {
      case IndAbsurd(target2, motive2) =>
        target.alphaEq(target2, thisMap, thatMap) &&
        motive.alphaEq(motive2, thisMap, thatMap)
      case _ => false
    }
  }

  /*
   ctx :- target <= Absurd
   ctx :- motive <= Universe
   -----------------
   ctx :- IndAbsurd (target, motive) => Universe
   */
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    for {
      target <- target.check(ctx, ValueAbsurd)
      motive <- motive.check(ctx, ValueUniverse)
    } yield The(Universe, IndAbsurd(target, motive))
  }
}

object IndAbsurd {
  def exe(
    target: Value,
    motive: Value,
  ): Either[ErrorMsg, Value] = {
    target match {
      case TheNeutral(ValueAbsurd, neutral) =>
        Right(
          TheNeutral(motive,
            NeutralIndAbsurd(neutral, TheValue(ValueUniverse, motive))))
      case _ =>
        Left(ErrorMsg(
          s"target should be TheNeutral(ValueAbsurd, neutral): ${target}"))
    }
  }
}
