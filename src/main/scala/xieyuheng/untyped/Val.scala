package xieyuheng.untyped

trait Val {
  def readback(used_names: Set[String]): Either[Err, Exp]
}
