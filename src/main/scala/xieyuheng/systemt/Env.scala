package xieyuheng.systemt

case class Env (map: Map[String, Value] = Map()) {
  def lookupValue(name: String): Option[Value] =
    map.get (name)

  def ext(name: String, value: Value): Env =
    Env(map + (name -> value))
}
