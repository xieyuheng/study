package xieyuheng.tt.tartlet

case class ValueQuote (sym: String) extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Quote(sym))
}
