package xieyuheng.tartlet

case class ValueQuote (sym: String) extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] =
    Right(Quote(sym))
}
