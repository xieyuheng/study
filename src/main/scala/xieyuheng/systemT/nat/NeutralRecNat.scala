package xieyuheng..systemT

case class NeutralRecNat (
  t: Type,
  target: Neutral,
  base: TheValue,
  step: TheValue,
) extends Neutral {
  def readBackNeutral(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      targetExp <- target.readBackNeutral(usedNames)
      baseExp <- base.readBackTheValue(usedNames)
      stepExp <- step.readBackTheValue(usedNames)
    } yield RecNat(t, targetExp, baseExp, stepExp)
  }
}
