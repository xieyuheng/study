package xieyuheng.tt.untyped

trait Exp {
  def eval(env: Env): Either[ErrorMsg, Value]
}
