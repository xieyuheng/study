package xieyuheng.systemT

case class NeutralRecNat (
  t: Type,
  target: Neutral,
  base: TheValue,
  step: TheValue,
) extends Neutral {
  def readbackNeutral(usedNames: Set[String]): Either[ErrorMsg, Exp] = {
    for {
      targetExp <- target.readbackNeutral(usedNames)
      baseExp <- base.readbackTheValue(usedNames)
      stepExp <- step.readbackTheValue(usedNames)
    } yield RecNat(t, targetExp, baseExp, stepExp)
  }
}
