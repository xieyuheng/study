package xieyuheng.systemt

trait Exp {
  def eval(env: Env): Either[Err, Value]

  def infer(ctx: Ctx): Either[Err, Type]

  def check(ctx: Ctx, t: Type): Either[Err, Unit]
}
