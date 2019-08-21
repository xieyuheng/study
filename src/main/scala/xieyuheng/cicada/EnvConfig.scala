package xieyuheng.cicada

case class EnvConfig(map: Map[String, String] = Map()) {
  def get(field: String) = map.get(field)
}

object EnvConfig {
  def default = EnvConfig(Map())
}
