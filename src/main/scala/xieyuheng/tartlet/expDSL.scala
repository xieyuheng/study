package xieyuheng.tartlet

object expDSL {
  implicit class ExpExtension(exp: Exp) {
    // def dot(fieldName: String): Dot = Dot(exp, fieldName)
    // def ap(mp: MultiMap[String, Exp]): Ap = Ap(exp, mp)
  }

  implicit class StringExtension(name: String) extends ExpExtension(Var(name))

  implicit def VarFromString(name: String): Var = Var(name)

  implicit def StringVarTupleFromStringStringTuple(kv: (String, String)) = {
    val (k, name) = kv
    (k, Var(name))
  }
}
