package xieyuheng.tt.tartlet

case class Ctx (map: Map[String, Den] = Map()) {
  def lookupDen(name: String): Option[Den] =
    map.get (name)

  def lookupType(name: String): Option[Value] =
    map.get (name) match {
      case Some(Def(t, value)) =>
        Some(t)
      case Some(Bind(t)) =>
        Some(t)
      case None =>
        None
    }

  def lookupValue(name: String): Option[Value] =
    map.get (name) match {
      case Some(Def(t, value)) =>
        Some(t)
      case Some(Bind(t)) =>
        None
      case None =>
        None
    }

  def ext(name: String, den: Den): Ctx =
    Ctx(map + (name -> den))

  def names: Set[String] = map.keySet

  def toEnv: Env = {
    val valueMap = map.map {
      case (name, Def(t, value)) => (name, value)
      case (name, Bind(t)) => (name, TheNeutral(t, NeutralVar(name)))
    }
    Env(valueMap)
  }
}
