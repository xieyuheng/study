package xieyuheng.cicada


case class Env(map: Map[String, EnvEntry])

sealed trait EnvEntry
final case class EnvEntryLet(name: String, value: Val) extends EnvEntry
