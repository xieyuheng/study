package xieyuheng.tt.tartlet

case object ValueAtom extends Value {
  def readBack (ctx: Ctx, t: Value): Either[ErrorMsg, Exp] =
    Right(Atom)
}
