package cicada

import cicada.pretty._

// subtype relation, and not just subtype relation,
//   because a value pre its type.

object pre {
  def apply(x: Value, y: Value, bind: Bind, env: Env): Boolean = {
    val (z, newBind) = join.yieldBind(x, y, bind, env)
    z == walk.deep(y, newBind)
  }
}
