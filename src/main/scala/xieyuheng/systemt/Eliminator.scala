package xieyuheng.systemt

trait Eliminator extends Exp {
  /** Eliminator Exp must provide infer method */

  /** Eliminator Exp have default check method */
  /*
   ctx :- e => T2
   T2 == T
   -----------------
   ctx :- e <= T
   */
  def check(ctx: Ctx, t: Type): Either[Err, Unit] = {
    for {
      t2 <- this.infer(ctx)
    } yield if (t == t2) {
      ()
    } else {
      Err(s"check is not implemented for exp: ${this}")
    }
  }
}
