package xieyuheng.tartlet

case class ValQuote (sym: String) extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Quote(sym))
}
