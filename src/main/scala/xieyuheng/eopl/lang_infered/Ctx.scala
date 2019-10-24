package xieyuheng.eopl.lang_infered

case class Ctx(map: Map[String, Type] = Map()) {

  def lookup_type(name: String): Option[Type] = {
    map.get(name)
  }

  def ext(name: String, t: Type): Ctx = {
    Ctx(map + (name -> t))
  }

}
