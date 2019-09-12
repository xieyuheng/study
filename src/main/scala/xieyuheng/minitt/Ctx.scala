package xieyuheng.minitt

sealed trait Ctx
final case class PatCtx(pat: Pat, value: Val, rest: Ctx) extends Ctx
final case class EmptyCtx() extends Ctx

object Ctx {
  def extend(pat: Pat, value: Val, rest: Ctx): Ctx = {
    // TODO
    PatCtx(pat: Pat, value: Val, rest: Ctx)
  }
}
