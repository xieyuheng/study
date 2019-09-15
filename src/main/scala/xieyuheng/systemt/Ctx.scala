package xieyuheng.systemt

case class Ctx(map: Map[String, Type] = Map()) {
  def lookup_type(name: String): Option[Type] =
    map.get(name)

  def names: Set[String] = map.keySet

  def ext(name: String, t: Type): Ctx =
    Ctx(map + (name -> t))
}
