package xieyuheng.untyped

case class Lambda (
  name: String,
  body: Exp,
) extends Exp {
  def eval(env: Env): Either[Err, Value] =
    Right(Closure(env, name, body))
}
