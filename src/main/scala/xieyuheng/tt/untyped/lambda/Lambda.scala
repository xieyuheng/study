package xieyuheng.tt.untyped

case class Lambda (
  name: String,
  body: Exp,
) extends Exp {
  def eval(env: Env): Either[ErrorMsg, Value] =
    Right(Closure(env, name, body))
}
