package xieyuheng.eopl.lang_checked

sealed trait Val
final case class ValNum(num: Int) extends Val
final case class ValBool(bool: Boolean) extends Val
final case class ValFn(name: String, arg_t: Type, body: Exp, env: Env) extends Val
final case class ValSole() extends Val
