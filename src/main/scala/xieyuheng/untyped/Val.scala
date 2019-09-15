package xieyuheng.untyped

sealed trait Val
sealed case class Closure(env: Env, name: String, body: Exp) extends Val

sealed trait Neu extends Val
final case class NeuVar(name: String) extends Neu
final case class NeuAp(fn: Neu, arg: Val) extends Neu
