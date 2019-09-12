package xieyuheng.minitt

object check {

  def check_decl(i: Int, env: Env, ctx: Ctx, decl: Decl): Either[Err, Ctx] = {
    decl match {
      case Let(pat, t_exp, e) =>
        for {
          _ <- check_type(i, env, ctx, t_exp)
          t = eval(t_exp, env)
          ctx1 <- ctx.ext(pat, t, eval(e, env))
        } yield ctx1
      case decl @ Letrec(pat, t_exp, e) =>
        for {
          _ <- check_type(i, env, ctx, t_exp)
          t = eval(t_exp, env)
          fresh = ValNeu(NeuVar(readback.fresh_name(i)))
          ctx1 <- ctx.ext(pat, t, fresh)
          _ <- check(i + 1, EnvPat(pat, fresh, env), ctx1, e, t)
          v = eval(e, EnvDecl(decl, env))
          ctx2 <- ctx.ext(pat, t, v)
        } yield ctx2
    }
  }

  def check_type(i: Int, env: Env, ctx: Ctx, t: Exp): Either[Err, Unit] = {
    ???
  }

  def check(i: Int, env: Env, ctx: Ctx, e: Exp, t: Val): Either[Err, Unit] = {
    ???
  }

  def check_infer(i: Int, env: Env, ctx: Ctx, e: Exp): Either[Err, Val] = {
    ???
  }

}
