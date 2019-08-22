package xieyuheng.untyped

trait Exp {
  def eval(env: Env): Either[ErrorMsg, Value]
}
