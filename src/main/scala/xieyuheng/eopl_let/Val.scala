package xieyuheng.eopl_let

sealed trait Val
final case class ValNum(num: Int) extends Val
final case class ValBool(bool: Boolean) extends Val
