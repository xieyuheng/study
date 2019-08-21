package xieyuheng

package object cicada {
  type Bind = Map[Id, Value]

  object Bind {
    def apply(): Bind = Map()
  }
}
