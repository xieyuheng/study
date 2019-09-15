package xieyuheng.systemt

sealed trait Val
final case class TheNeu(t: Type, neu: Neu) extends Val
final case class ValSucc(prev: Val) extends Val
final case object ValZero extends Val
final case class ValFn(env: Env, name: String, body: Exp) extends Val

sealed trait Neu
final case class NeuVar(name: String) extends Neu
final case class NeuAp(fn: Neu, arg: TheVal) extends Neu
final case class NeuRecNat(t: Type, target: Neu, base: TheVal, step: TheVal) extends Neu

final case class TheVal(t: Type, value: Val)
