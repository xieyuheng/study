package xieyuheng.minitt

object check {

  def check_decl(i: Int, env: Env, ctx: Ctx, decl: Decl): Either[Err, Ctx] = {
    ???
  }

  def check_univ(i: Int, env: Env, ctx: Ctx, t: Exp): Either[Err, Unit] = {
    ???
  }

  def check_type(i: Int, env: Env, ctx: Ctx, e: Exp, t: Val): Either[Err, Unit] = {
    ???
  }

  def check_infer(i: Int, env: Env, ctx: Ctx, e: Exp): Either[Err, Val] = {
    ???
  }

}
