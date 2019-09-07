package xieyuheng.minitt

object expDSL {
  def choice(pairs: (String, Exp)*): Choice = Choice(Map(pairs: _*))

  def sum(pairs: (String, Exp)*): Sum = Sum(Map(pairs: _*))

  def fn(patterns: Pattern*)(body: Exp): Exp = {
    var exp = body
    patterns.reverse.foreach { case pattern =>
      exp = Fn(pattern, exp)
    }
    exp
  }

  def pi(pair: (Pattern, Exp))(ret: Exp): Pi = {
    val (pattern, arg) = pair
    Pi(pattern, arg, ret)
  }

  def %(tag: String, body: Exp = Sole): Data = Data(tag, body)

  implicit class ExpExtension(exp: Exp) {
    def ->:(arg: Exp) = Pi(EmptyPattern, arg, exp)
    def *(cdr: Exp) = Sigma(EmptyPattern, exp, cdr)
    def $(arg: Exp) = Ap(exp, arg)
    def :: (pattern: Pattern): (Pattern, Exp) = (pattern, exp)
  }

  implicit class StringExtension(string: String) extends ExpExtension(Var(string))

  implicit def VarFromString(name: String) = Var(name)

  implicit def VarPatternFromString(name: String) = VarPattern(name)
}
