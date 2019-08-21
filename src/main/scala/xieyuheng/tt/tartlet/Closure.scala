package xieyuheng.tt.tartlet

sealed trait Closure {
  def name: String
  def apply(value: Value): Either[ErrorMsg, Value]
}

final case class NativeClosure (
  name: String,
  fun: Value => Either[ErrorMsg, Value],
) extends Closure {
  def apply(value: Value): Either[ErrorMsg, Value] =
    fun(value)
}

final case class EnvClosure (
  env: Env,
  name: String,
  body: Exp,
) extends Closure {
  def apply(value: Value): Either[ErrorMsg, Value] =
    body.eval (env.ext (name, value))
}
