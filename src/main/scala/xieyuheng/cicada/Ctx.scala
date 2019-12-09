package xieyuheng.cicada

case class Ctx(map: Map[String, Exp] = Map()) {

  def lookup_type(name: String): Option[Exp] = {
    map.get(name)
  }

  def ext(name: String, t: Exp): Ctx = {
    Ctx(this.map + (name -> t))
  }

  def ext_map(map: Map[String, Exp]): Ctx = {
    Ctx(this.map ++ map)
  }

}
