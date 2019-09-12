package xieyuheng.minitt

object check {

  def checkDeclAndExtend(env: Env, ctx: Ctx, decl: Decl): Ctx = ???

  def checkTypeOfType(env: Env, ctx: Ctx, t: Exp): Boolean = ???

  def check(env: Env, ctx: Ctx, e: Exp, t: Val): Boolean = ???

  def infer(env: Env, ctx: Ctx, e: Exp): Val = ???

}
