package xieyuheng.adventure.jojo_untyped

sealed trait EnvEntry
final case class EnvEntryLet(value: Val) extends EnvEntry
final case class EnvEntryDefine(value: Val) extends EnvEntry

case class Env(map: Map[String, EnvEntry] = Map()) {

  def lookup_val(name: String): Option[EnvEntry] = {
    map.get(name)
  }

  def ext(name: String, entry: EnvEntry): Env = {
    Env(map + (name -> entry))
  }

  def ext_let(name: String, value: Val): Env = {
    ext(name, EnvEntryLet(value))
  }

  def ext_define(name: String, value: Val): Env = {
    ext(name, EnvEntryDefine(value))
  }
}
