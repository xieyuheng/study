package xieyuheng.cicada.with_logic_variable

import xieyuheng.cicada.with_logic_variable.pretty._

// subtype relation, and not just subtype relation,
//   because a value pre its type.

object pre {
  def apply(x: Val, y: Val, bind: Bind, env: Env): Boolean = {
    val (z, newBind) = join.yieldBind(x, y, bind, env)
    z == walk.deep(y, newBind)
  }
}
