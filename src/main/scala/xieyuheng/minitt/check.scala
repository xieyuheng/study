package xieyuheng.minitt

object check {

  def checkDeclExt(env: Env, ctx: Ctx, decl: Decl): Ctx = ???

  def checkUniv(env: Env, ctx: Ctx, t: Exp): Boolean = ???

  def checkType(env: Env, ctx: Ctx, e: Exp, t: Val): Boolean = ???

  def infer(env: Env, ctx: Ctx, e: Exp): Val = ???

}
