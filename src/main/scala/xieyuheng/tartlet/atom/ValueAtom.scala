package xieyuheng.tartlet

case object ValueAtom extends Value {
  def readback (ctx: Ctx, t: Value): Either[Err, Exp] =
    Right(Atom)
}
