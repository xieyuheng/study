package xieyuheng.adventure.untyped_minimal

import scala.annotation.tailrec

sealed trait Env {

  @tailrec
  def lookup_val(name: String): Option[Val] = {
    val env = this
    env match {
      case EnvEmpty() =>
        None
      case EnvLet(name2: String, value: Val, rest: Env) =>
        if (name == name2) {
          Some(value)
        } else {
          rest.lookup_val(name)
        }
    }
  }

  def ext_let(name: String, value: Val): Env = {
    val rest = this
    EnvLet(name, value, rest)
  }
}

final case class EnvEmpty() extends Env
final case class EnvLet(name: String, value: Val, rest: Env) extends Env
