package xieyuheng.cicada

case class Env(val defMap: Map[String, Def] = Map()) {
  def get(name: String): Option[Def] =
    defMap.get(name)

  def extend(kv: (String, Def)): Env =
    Env(defMap + kv)
}
