package xieyuheng.cicada

object util {
  def newId(): String = {
    java.util.UUID.randomUUID().toString
  }
}
