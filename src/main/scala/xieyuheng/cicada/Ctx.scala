package xieyuheng.cicada

case class Ctx(map: Map[String, Val] = Map()) {

  def lookup_type(name: String): Option[Val] = {
    map.get(name)
  }

  def ext(name: String, t: Val): Ctx = {
    Ctx(this.map + (name -> t))
  }

  def ext_map(map: Map[String, Val]): Ctx = {
    Ctx(this.map ++ map)
  }

}
