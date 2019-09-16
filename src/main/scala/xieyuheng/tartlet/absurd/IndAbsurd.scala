package xieyuheng.tartlet

case class AbsurdInd (
  target: Exp,
  motive: Exp,
) extends Eliminator {
  def eval(env: Env): Either[Err, Val] =
    for {
      targetVal <- target.eval(env)
      motiveVal <- motive.eval(env)
      res <- AbsurdInd.exe(targetVal, motiveVal)
    } yield res

  def alpha_eq(
    that: Exp,
    this_map: Map[String, String],
    that_map: Map[String, String],
  ): Boolean = {
    that match {
      case AbsurdInd(target2, motive2) =>
        target.alpha_eq(target2, this_map, that_map) &&
        motive.alpha_eq(motive2, this_map, that_map)
      case _ => false
    }
  }

  /*
   ctx :- target <= Absurd
   ctx :- motive <= Universe
   -----------------
   ctx :- AbsurdInd (target, motive) => Universe
   */
  def infer(ctx: Ctx): Either[Err, The] = {
    for {
      target <- target.check(ctx, ValAbsurd)
      motive <- motive.check(ctx, ValUniverse)
    } yield The(Universe, AbsurdInd(target, motive))
  }
}

object AbsurdInd {
  def exe(
    target: Val,
    motive: Val,
  ): Either[Err, Val] = {
    target match {
      case TheNeu(ValAbsurd, neutral) =>
        Right(
          TheNeu(motive,
            NeuAbsurdInd(neutral, TheVal(ValUniverse, motive))))
      case _ =>
        Left(Err(
          s"target should be TheNeu(ValAbsurd, neutral): ${target}"))
    }
  }
}
