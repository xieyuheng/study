package xieyuheng.systemt

trait Val {
  def readback (used_names: Set[String], t: Type): Either[Err, Exp]
}
