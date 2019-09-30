package xieyuheng.cicada.telescope

import scala.annotation.tailrec

import eval._
import pretty._

sealed trait Ctx {

  @tailrec
  def lookup_type(key: String): Option[Val] = {
    val ctx = this
    ctx match {
      case CtxEmpty() => None
      case CtxName(name, t, rest) =>
        if (key == name) {
          Some(t)
        } else {
          rest.lookup_type(key)
        }
    }
  }

  def ext_type(name: String, t: Val): Ctx = {
    CtxName(name, t, this)
  }

}

final case class CtxName(name: String, t: Val, rest: Ctx) extends Ctx
final case class CtxEmpty() extends Ctx

object Ctx {
  def apply(): Ctx = CtxEmpty()
}
