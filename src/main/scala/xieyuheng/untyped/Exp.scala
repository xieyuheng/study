package xieyuheng.untyped

trait Exp {
  def eval(env: Env): Either[Err, Val]
}
