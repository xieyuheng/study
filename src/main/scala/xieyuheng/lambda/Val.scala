package xieyuheng.lambda

sealed trait Val
sealed case class ValFn(name: String, body: Exp, env: Env) extends Val

sealed trait Neu extends Val
final case class NeuVar(name: String) extends Neu
final case class NeuAp(fn: Neu, arg: Val) extends Neu
