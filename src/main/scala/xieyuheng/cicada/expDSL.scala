package xieyuheng.cicada

/**
  * Exp Conveniences
  * (for testing only)
  */

object expDSL {

  def %[K, V](entries: (K, V)*): MultiMap[K, V] = new MultiMap(entries.toList)
  def the(exp: Exp) = The(exp)
  def the_type = Type()
  def choice(target: Exp, map: MultiMap[String, Exp]) = Choice(target, map)
  def pi(args: MultiMap[String, Exp], ret: Exp) = Pi(args, ret)
  def fn(args: MultiMap[String, Exp], ret: Exp, body: Exp) = Fn(args, ret, body)

  implicit class ExpExtension(exp: Exp) {
    def dot(fieldName: String): Dot = Dot(exp, fieldName)
    def ap(mp: MultiMap[String, Exp]): Ap = Ap(exp, mp)
    def :: (name: String): (String, The) = (name, The(exp))
    def =: (name: String): (String, Exp) = (name, exp)
  }

  implicit class StringExtension(name: String) extends ExpExtension(Var(name)) {
    def := (exp: Exp): (String, Exp) = (name, exp)
  }

  implicit def VarFromString(name: String): Var = Var(name)

  implicit def StringVarTupleFromStringStringTuple(kv: (String, String)): (String, Var) = {
    val (k, name) = kv
    (k, Var(name))
  }
}
