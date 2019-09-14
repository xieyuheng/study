package xieyuheng.systemt

trait Neu {
  def readback_neu (used_names: Set[String]): Either[Err, Exp]
}
