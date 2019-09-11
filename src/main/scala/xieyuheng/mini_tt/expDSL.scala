package xieyuheng.mini_tt

object expDSL {
  def mat(pairs: (String, Exp)*): Mat = Mat(Map(pairs: _*))

  def sum(pairs: (String, Exp)*): Sum = Sum(Map(pairs: _*))

  def cons(car: Exp, cdr: Exp): Exp = Cons(car, cdr)

  def cons(car: Pattern, cdr: Pattern): Pattern = ConsPattern(car, cdr)

  def fn(patterns: Pattern*)(body: Exp): Exp = {
    var exp = body
    patterns.reverse.foreach { case pattern =>
      exp = Fn(pattern, exp)
    }
    exp
  }

  def pi(pair: (Pattern, Exp))(t: Exp): Pi = {
    val (pattern, arg) = pair
    Pi(pattern, arg, t)
  }

  def %(tag: String, body: Exp = Sole): Data = Data(tag, body)

  implicit class PatternExtension(pattern: Pattern) {
    def *(cdr: Pattern): Pattern = ConsPattern(pattern, cdr)
  }

  implicit class ExpExtension(exp: Exp) {
    def ->:(arg: Exp): Exp = Pi(SolePattern, arg, exp)
    def **(t: Exp): Exp = Sigma(SolePattern, exp, t)
    def *(cdr: Exp): Exp = Cons(exp, cdr)
    def $(arg: Exp): Exp = Ap(exp, arg)
    def ::(pattern: Pattern): (Pattern, Exp) = (pattern, exp)
  }

  implicit class StringExtension(string: String) extends ExpExtension(Var(string)) {
    def *(cdr: String): Pattern = ConsPattern(VarPattern(string), VarPattern(cdr))
  }

  implicit def VarFromString(name: String) = Var(name)

  implicit def VarPatternFromString(name: String) = VarPattern(name)
}
