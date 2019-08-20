package xieyuheng

package object cicada {
  type Id = String

  type Bind = Map[Id, Value]

  object Bind {
    def apply(): Bind = Map()
  }
}
