package xieyuheng.adventure.jojo_untyped

sealed trait EnvEntry
final case class EnvEntryLet(value: Val) extends EnvEntry
final case class EnvEntryDefine(value: Val) extends EnvEntry

case class Env(map: Map[String, EnvEntry] = Map()) {

  def lookup(name: String): Option[EnvEntry] = {
    map.get(name)
  }

  def ext(name: String, entry: EnvEntry): Env = {
    Env(map + (name -> entry))
  }

}
