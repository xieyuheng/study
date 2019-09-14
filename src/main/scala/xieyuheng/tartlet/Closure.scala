package xieyuheng.tartlet

sealed trait Closure {
  def name: String
  def apply(value: Value): Either[Err, Value]
}

final case class NativeClosure (
  name: String,
  fn: Value => Either[Err, Value],
) extends Closure {
  def apply(value: Value): Either[Err, Value] =
    fn(value)
}

final case class EnvClosure (
  env: Env,
  name: String,
  body: Exp,
) extends Closure {
  def apply(value: Value): Either[Err, Value] =
    body.eval (env.ext (name, value))
}
