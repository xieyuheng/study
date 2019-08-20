package xieyuheng.cicada

case class Env(val defMap: Map[String, Def] = Map()) {
  def get(name: String): Option[Def] = {
    defMap.get(name)
  }

  def extend(kv: (String, Def)): Env = {
    Env(defMap + kv)
  }

  def defineValue(
    name: String,
    value: Value,
  ): Env = {
    extend(name -> DefineValue(name, value))
  }

  def defineRecord(
    name: String,
    map: MultiMap[String, Exp],
  ): Env = {
    extend(name -> DefineRecord(name, map))
  }

  def defineUnion(
    name: String,
    map: MultiMap[String, Exp],
    subNames: List[String],
  ): Env = {
    extend(name -> DefineUnion(name, map, subNames))
  }
}
