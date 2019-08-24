package xieyuheng.cicada

object join {
  def apply(x: Value, y: Value, bind: Bind, env: Env): Value = {
    val (z, _newBind) = yieldBind(x, y, bind, env)
    z
  }

  def yieldBind(x: Value, y: Value, bind: Bind, env: Env): (Value, Bind) = {
    unify(x, y, bind, env) match {
      case Right((newBind)) =>
        (walk.deep(x, newBind), newBind)
      case Left(_) => (TopValue(), bind)
    }
  }
}