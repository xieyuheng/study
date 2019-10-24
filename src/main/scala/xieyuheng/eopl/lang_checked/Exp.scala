package xieyuheng.eopl.lang_checked

sealed trait Exp
final case class Var(name: String) extends Exp
final case class Num(num: Int) extends Exp
final case class Diff(exp1: Exp, exp2: Exp) extends Exp
final case class ZeroP(exp1: Exp) extends Exp
final case class If(exp1: Exp, exp2: Exp, exp3: Exp) extends Exp
final case class Let(name: String, exp1: Exp, body: Exp) extends Exp
final case class Fn(name: String, arg_t: Type, body: Exp) extends Exp
final case class Ap(target: Exp, arg: Exp) extends Exp
final case class LetRec(fn_name: String, arg_name: String, arg_t: Type, ret_t: Type, fn_body: Exp, body: Exp) extends Exp
final case class Sole() extends Exp
final case class Do(exp1: Exp, body: Exp) extends Exp
final case class AssertEq(exp1: Exp, exp2: Exp) extends Exp
final case class Show(exp1: Exp) extends Exp
