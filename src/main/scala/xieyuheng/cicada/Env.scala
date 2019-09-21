package xieyuheng.cicada

case class Env(map: Map[String, Val]) {

  def lookup_val(name: String): Option[Val] =
    map.get(name)

  def ext(name: String, value: Val): Env =
    Env(map + (name -> value))

  def ext_by_decl(decl: Decl): Env = {
    ???
    // Env(map + (name -> value))
  }

}
