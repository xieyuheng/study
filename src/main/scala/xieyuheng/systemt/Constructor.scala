package xieyuheng.systemt

trait Constructor extends Exp {
  /** Constructor Exp must provide check method */

  /** Constructor Exp have default infer method */
  def infer(ctx: Ctx): Either[Err, Type] = {
    Left(Err(s"infer is not implemented for exp: ${this}"))
  }
}
