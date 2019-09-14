package xieyuheng.tartlet

case class IndAbsurd (
  target: Exp,
  motive: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] =
    for {
      targetVal <- target.eval(env)
      motiveVal <- motive.eval(env)
      res <- IndAbsurd.exe(targetVal, motiveVal)
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
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      target <- target.check(ctx, ValAbsurd)
      motive <- motive.check(ctx, ValUniverse)
    } yield The(Universe, IndAbsurd(target, motive))
  }
}

object IndAbsurd {
  def exe(
    target: Val,
    motive: Val,
  ): Either[Err, Val] = {
    target match {
      case TheNeu(ValAbsurd, neutral) =>
        Right(
          TheNeu(motive,
            NeuIndAbsurd(neutral, TheVal(ValUniverse, motive))))
      case _ =>
        Left(Err(
          s"target should be TheNeu(ValAbsurd, neutral): ${target}"))
    }
  }
}
