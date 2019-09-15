package xieyuheng.lambda

object eval {

  def apply(exp: Exp, env: Env): Val =
    eval(exp: Exp, env: Env)

  def eval(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name) =>
        env.lookup_val(name) match {
          case Some(value) => value
          case None =>
            println((s"can not find var: ${name}"))
            throw new Exception()
        }
      case Ap(rator: Exp, rand: Exp) =>
        val fn = eval(rator, env)
        val arg = eval(rand, env)
        fn match {
          case fn: ValFn =>
            eval(fn.body, fn.env.ext(fn.name, arg))
          case fn: Neu =>
            NeuAp(fn, arg)
          case _ =>
            s"unknown fn value type: ${fn}"
            throw new Exception()
        }
      case Fn(name, body) =>
        ValFn(name, body, env)
      case Block(DeclLet(name, e), body) =>
        val fn = Fn(name, body)
        val ap = Ap(fn, e)
        eval(ap, env)
    }
  }

}
