package xieyuheng.cicada

/**
  * Exp Conveniences
  * (for testing only)
  */

object expDSL {

  object $ {
    def apply[K, V](entries: (K, V)*): MultiMap[K, V] =
      new MultiMap(entries.toList)
  }

  implicit class ExpExtension(exp: Exp) {
    def dot(fieldName: String): Dot = Dot(exp, fieldName)

    def ap(mp: MultiMap[String, Exp]): Ap = {
      Ap(exp, mp)
    }
  }

  implicit class StringExtension(name: String) extends ExpExtension(Var(name))

  implicit def VarFromString(name: String): Var = {
    Var(name)
  }

  implicit def StringVarTupleFromStringStringTuple(kv: (String, String)) = {
    val (k, name) = kv
    (k, Var(name))
  }
}
