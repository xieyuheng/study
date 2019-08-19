package xieyuheng

package object cicada {
  type Id = String

  type Env = Map[String, Def]

  type Bind = Map[Id, Value]

  object Bind {
    def apply(): Bind = Map[Id, Value]()
  }
}
