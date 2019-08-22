package xieyuheng.systemT

case class Ctx (map: Map[String, Type] = Map()) {
  def lookupType(name: String): Option[Type] =
    map.get (name)

  def names: Set[String] = map.keySet

  def ext(name: String, t: Type): Ctx =
    Ctx(map + (name -> t))
}
