package xieyuheng.systemt

case class NeuRecNat (
  t: Type,
  target: Neu,
  base: TheVal,
  step: TheVal,
) extends Neu {
  def readback_neu(used_names: Set[String]): Either[Err, Exp] = {
    for {
      targetExp <- target.readback_neu(used_names)
      baseExp <- base.readback_the_val(used_names)
      stepExp <- step.readback_the_val(used_names)
    } yield RecNat(t, targetExp, baseExp, stepExp)
  }
}
