package xieyuheng.tartlet

sealed trait Clo {
  def name: String
  def apply(value: Val): Either[Err, Val]
}

final case class NativeClo (
  name: String,
  fn: Val => Either[Err, Val],
) extends Clo {
  def apply(value: Val): Either[Err, Val] =
    fn(value)
}

final case class EnvClo (
  env: Env,
  name: String,
  body: Exp,
) extends Clo {
  def apply(value: Val): Either[Err, Val] =
    body.eval (env.ext (name, value))
}
