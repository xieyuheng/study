package xieyuheng.untyped

trait Val {
  def readback(usedNames: Set[String]): Either[Err, Exp]
}
