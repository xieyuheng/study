package cicada

import java.util.UUID

case class Id(name: String = "") {
  val uuid: UUID = UUID.randomUUID()

  override def toString(): String = {
    name ++ "#" ++ uuid.toString
  }

  override def equals(that: Any): Boolean = {
    that match {
      case id: Id =>
        name == id.name && uuid == id.uuid
      case _ =>
        false
    }
  }
}
