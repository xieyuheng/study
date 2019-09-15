package xieyuheng.tartlet

case class ValQuote (sym: String) extends Val {
  def readback_val(ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Quote(sym))
}
