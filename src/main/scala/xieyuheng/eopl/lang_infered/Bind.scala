package xieyuheng.eopl.lang_infered

case class Bind(map: Map[Int, Type] = Map()) {

  def lookup_type(serial: Int): Option[Type] = {
    map.get(serial)
  }

  def ext(serial: Int, arg_t: Type): Bind = {
    Bind(map + (serial -> arg_t))
  }

  def map(f: ((Int, Type)) => (Int, Type)): Bind = {
    Bind(map.map(f))
  }

}
