package xieyuheng.untyped

trait Value {
  def readback(usedNames: Set[String]): Either[Err, Exp]
}
