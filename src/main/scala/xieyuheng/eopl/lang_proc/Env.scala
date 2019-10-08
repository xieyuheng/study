package xieyuheng.eopl.lang_proc

import scala.annotation.tailrec

sealed trait Env {

  @tailrec
  def lookup_val(name: String): Option[Val] = {
    val env = this
    env match {
      case EnvEmpty() =>
        None
      case EnvName(name2: String, value: Val, rest: Env) =>
        if (name == name2) {
          Some(value)
        } else {
          rest.lookup_val(name)
        }
    }
  }

  def ext_name(name: String, value: Val): Env = {
    val rest = this
    EnvName(name, value, rest)
  }

}

final case class EnvEmpty() extends Env
final case class EnvName(name: String, value: Val, rest: Env) extends Env
