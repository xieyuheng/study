package xieyuheng.minitt

object expDSL {
  def mat(pairs: (String, Exp)*): Mat = Mat(Map(pairs: _*))

  def sum(pairs: (String, Exp)*): Sum = Sum(Map(pairs: _*))

  def cons(car: Exp, cdr: Exp): Exp = Cons(car, cdr)

  def cons(car: Pat, cdr: Pat): Pat = PatCons(car, cdr)

  def __ = PatSole()

  def fn(pats: Pat*)(body: Exp): Exp = {
    pats.foldRight(body) { case (pat, exp) => Fn(pat, exp) }
  }

  def pi(pair: (Pat, Exp))(t: Exp): Pi = {
    val (pat, arg) = pair
    Pi(pat, arg, t)
  }

  def %(tag: String, body: Exp = Sole()): Data = Data(tag, body)

  implicit class PatExtension(pat: Pat) {
    def *(cdr: Pat): Pat = PatCons(pat, cdr)
  }

  implicit class ExpExtension(exp: Exp) {
    def ->:(arg: Exp): Exp = Pi(PatSole(), arg, exp)
    def **(t: Exp): Exp = Sigma(PatSole(), exp, t)
    def *(cdr: Exp): Exp = Cons(exp, cdr)
    def $(arg: Exp): Exp = Ap(exp, arg)
    def ::(pat: Pat): (Pat, Exp) = (pat, exp)
  }

  implicit class StringExtension(string: String) extends ExpExtension(Var(string)) {
    def *(cdr: String): Pat = PatCons(PatVar(string), PatVar(cdr))
  }

  implicit def VarFromString(name: String) = Var(name)

  implicit def PatVarFromString(name: String) = PatVar(name)
}
