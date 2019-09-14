package xieyuheng.cicada.with_logic_variable

object join {
  def apply(x: Val, y: Val, bind: Bind, env: Env): Val = {
    val (z, _newBind) = yieldBind(x, y, bind, env)
    z
  }

  def yieldBind(x: Val, y: Val, bind: Bind, env: Env): (Val, Bind) = {
    unify(x, y, bind, env) match {
      case Right((newBind)) =>
        (walk.deep(x, newBind), newBind)
      case Left(_) => (TopVal(), bind)
    }
  }
}
