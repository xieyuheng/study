package xieyuheng.cicada

object eval {

  def eval(exp: Exp, env: Env): Val = {
    exp match {
      case Var(name: String) =>
        env.lookup_val(name) match {
          case Some(value) =>
            value
          case None =>
            println(s"can not find var: ${name} in env: ${env}")
            throw new Exception()
        }
      case Type(level: Int) =>
        ValType(level)
      // case The(t: Exp, body: Exp) =>
      //   ValThe(eval(t, env), eval(body, env))
      case Pi(arg_name: String, arg_t: Exp, dep_t: Exp) =>
        ValPi(arg_name, eval(arg_t, env), Clo(arg_name, dep_t, env))
      case Fn(arg_name: String, arg_t: Exp, body: Exp) =>
        ValFn(arg_name, eval(arg_t, env), Clo(arg_name, body, env))
      case Ap(target: Exp, arg: Exp) =>
        ???
      case Choice(target: Exp, map: Map[String, Exp]) =>
        ???
      case Dot(target: Exp, field_name: String) =>
        ???
      case DotType(target: Exp, field_name: String) =>
        ???
      case Let(decl: Decl, body: Exp) =>
        ???
    }
  }

}
