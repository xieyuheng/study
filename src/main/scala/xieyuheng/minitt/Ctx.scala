package xieyuheng.minitt

sealed trait Ctx
final case class PatternCtx(pattern: Pattern, value: Val, rest: Ctx) extends Ctx
final case class EmptyCtx() extends Ctx

object Ctx {
  def extend(pattern: Pattern, value: Val, rest: Ctx): Ctx = {
    // TODO
    PatternCtx(pattern: Pattern, value: Val, rest: Ctx)
  }
}
