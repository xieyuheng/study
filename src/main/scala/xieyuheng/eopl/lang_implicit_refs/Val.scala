package xieyuheng.eopl.lang_implicit_refs

sealed trait Val
final case class ValNum(num: Int) extends Val
final case class ValBool(bool: Boolean) extends Val
final case class ValFn(name: String, body: Exp, env: Env) extends Val
final case class ValSole() extends Val
final case class ValRef(address: Int) extends Val
