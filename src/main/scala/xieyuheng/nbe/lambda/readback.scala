package xieyuheng.lambda

object readback {
  def readback_val(value: Val, used_names: Set[String]): Exp = {
    value match {
      case ValFn(name: String, body: Exp, env: Env) =>
        val fresh_name = freshen(used_names, name)
        val value = eval(body, env.ext(name, NeuVar(fresh_name)))
        Fn(fresh_name, readback_val(value, used_names + fresh_name))
      case NeuVar(name: String) =>
        Var(name)
      case NeuAp(fn: Neu, arg: Val) =>
        Ap(readback_val(fn, used_names), readback_val(arg, used_names))
    }
  }

}
