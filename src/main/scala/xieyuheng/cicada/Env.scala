package xieyuheng.cicada

import scala.collection.immutable.ListMap

import xieyuheng.cicada.pretty._

case class Env(map: Map[String, Define] = Map()) {
  def get(name: String): Option[Define] = {
    map.get(name)
  }

  def contains(name: String): Boolean = {
    get(name).isDefined
  }

  def extend(kv: (String, Define)): Env = {
    Env(map + kv)
  }

  def extendByValueMap(valueMap: ListMap[String, Value]): Env = {
    valueMap.foldLeft(this) { case (env, (name, value)) =>
      env.extend(name -> DefineValue(name, value))
    }
  }
}
