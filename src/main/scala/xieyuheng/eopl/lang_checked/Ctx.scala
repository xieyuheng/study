package xieyuheng.eopl.lang_checked

case class Ctx(map: Map[String, Type] = Map()) {

  def lookup_type(name: String): Option[Type] = {
    map.get(name)
  }

  def ext(name: String, t: Type): Ctx = {
    Ctx(map + (name -> t))
  }

  def ext_map(map: Map[String, Type]): Ctx = {
    Ctx(this.map ++ map)
  }

}
