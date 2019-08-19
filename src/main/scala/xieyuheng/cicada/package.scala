package xieyuheng

package object cicada {
  type Id = String

  type Env = Map[String, Def]

  object Env {
    def apply(): Env = Map()
  }

  type Bind = Map[Id, Value]

  object Bind {
    def apply(): Bind = Map()
  }
}
