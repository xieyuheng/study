package xieyuheng.cicada

object eval {

  def apply(exp: Exp, env: Env): Val =
    eval(exp: Exp, env: Env)

  def eval(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name) =>
        env.lookup_val(name) match {
          case Some(value) =>
            value
          case None =>
            println(s"can not find var: ${name} in env: ${env}")
            throw new Exception()
        }
      case Type(level) =>
        ValType(level)
      // case The(t: Exp, body: Exp) =>
      //   ValThe(eval(t, env), eval(body, env))
      case Pi(arg_name, arg_t, dep_t) =>
        ValPi(arg_name, eval(arg_t, env), Clo(arg_name, dep_t, env))
      case Fn(arg_name, arg_t, body) =>
        ValFn(arg_name, eval(arg_t, env), Clo(arg_name, body, env))
      case Ap(target, arg) =>
        Ap.ap(eval(target, env), eval(arg, env))
      case Choice(path, map: Map[String, Exp]) =>
        Choice.ap(path, map, env)
      case Dot(target, field_name) =>
        Dot.ap(eval(target, env), field_name)
      case DotType(target, field_name) =>
        DotType.ap(eval(target, env), field_name)
      case Let(decl, body) =>
        eval(body, env.ext_decl(decl))
    }
  }

}
