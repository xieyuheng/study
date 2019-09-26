package xieyuheng.cicada

import readback._

case class Clo(arg_name: String, arg_t: Val, body: Exp, env: Env) {
  def apply(arg: Val): Val = {
    eval(body, env.ext_val(arg_name, arg))
  }

  def force(): Val = {
    val clo = this
    clo(gen_neu_val(clo.arg_name, clo.arg_t, None))
  }
}
