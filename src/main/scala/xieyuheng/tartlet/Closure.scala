package xieyuheng.tartlet

sealed trait Closure {
  def name: String
  def apply(value: Val): Either[Err, Val]
}

final case class NativeClosure (
  name: String,
  fn: Val => Either[Err, Val],
) extends Closure {
  def apply(value: Val): Either[Err, Val] =
    fn(value)
}

final case class EnvClosure (
  env: Env,
  name: String,
  body: Exp,
) extends Closure {
  def apply(value: Val): Either[Err, Val] =
    body.eval (env.ext (name, value))
}
