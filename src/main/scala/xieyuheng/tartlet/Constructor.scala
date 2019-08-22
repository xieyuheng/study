package xieyuheng..tartlet

trait Constructor extends Exp {
  /** Constructor Exp must provide check method */

  /** Constructor Exp have default infer method */
  def infer(ctx: Ctx): Either[ErrorMsg, The] = {
    Left(ErrorMsg(s"infer is not implemented for exp: ${this}"))
  }
}
