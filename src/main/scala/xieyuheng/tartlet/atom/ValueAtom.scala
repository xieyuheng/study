package xieyuheng.tartlet

case object ValAtom extends Val {
  def readback (ctx: Ctx, t: Val): Either[Err, Exp] =
    Right(Atom)
}
