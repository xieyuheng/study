package xieyuheng..systemT

trait Eliminator extends Exp {
  /** Eliminator Exp must provide infer method */

  /** Eliminator Exp have default check method */
  /*
   ctx :- e => T2
   T2 == T
   -----------------
   ctx :- e <= T
   */
  def check(ctx: Ctx, t: Type): Either[ErrorMsg, Unit] = {
    for {
      t2 <- this.infer(ctx)
    } yield if (t == t2) {
      ()
    } else {
      ErrorMsg(s"check is not implemented for exp: ${this}")
    }
  }
}
