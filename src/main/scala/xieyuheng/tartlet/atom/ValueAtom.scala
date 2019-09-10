package xieyuheng.tartlet

case object ValueAtom extends Value {
  def readback (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Atom)
}
