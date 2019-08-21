package xieyuheng.tt.systemT

trait Exp {
  def eval(env: Env): Either[ErrorMsg, Value]

  def infer(ctx: Ctx): Either[ErrorMsg, Type]

  def check(ctx: Ctx, t: Type): Either[ErrorMsg, Unit]
}
