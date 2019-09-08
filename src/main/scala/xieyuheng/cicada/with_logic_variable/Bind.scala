package xieyuheng.cicada.with_logic_variable

case class Bind(map: Map[Id, Value] = Map()) {
  def get(id: Id): Option[Value] = {
    map.get(id)
  }

  def contains(id: Id): Boolean = {
    get(id).isDefined
  }

  def extend(kv: (Id, Value)): Bind = {
    Bind(map + kv)
  }

  def size = map.size

  def keys = map.keys

  def toSet = map.toSet

  def extendByBind(that: Bind): Bind = {
    Bind(map ++ that.map)
  }

  def filter(p: ((Id, Value)) => Boolean): Bind = {
    Bind(map.filter(p))
  }

  def filterNot(p: ((Id, Value)) => Boolean): Bind = {
    Bind(map.filterNot(p))
  }
}
