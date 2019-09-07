package xieyuheng.minitt

case class Module() {
  var env: Env = EmptyEnv

  def let(pattern: Pattern, t: Exp, e: Exp): Unit = {
    env = DeclEnv(Let(pattern, t, e), env)
  }

  def letrec(pattern: Pattern, t: Exp, e: Exp): Unit = {
    env = DeclEnv(LetRec(pattern, t, e), env)
  }

  def import_all(module: Module): Unit = {
    env = Env.append(module.env, env)
  }

  def run(exp: Exp): Value = eval(exp, env)

}
