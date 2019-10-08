package xieyuheng.eopl.lang_nameless

sealed trait Val
final case class ValNum(num: Int) extends Val
final case class ValBool(bool: Boolean) extends Val
final case class ValFn(name: String, body: Idx, idx_env: IdxEnv) extends Val
