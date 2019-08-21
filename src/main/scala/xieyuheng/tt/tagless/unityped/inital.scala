package xieyuheng.tt.tagless.unityped.inital

sealed trait Exp
final case class Lit (int: Int) extends Exp
final case class Neg (x: Exp) extends Exp
final case class Add (x: Exp, y: Exp) extends Exp

object InitalApp extends App {

  def ti1: Exp = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

  def eval(exp: Exp): Int = {
    exp match {
      case Lit(int) => int
      case Neg(x) => -(eval(x))
      case Add(x, y) => eval(x) + eval(y)
    }
  }

  def view(exp: Exp): String = {
    exp match {
      case Lit(int) => s"${int}"
      case Neg(x) => s"-${view(x)}"
      case Add(x, y) => s"(${view(x)}+${view(y)})"
    }
  }

  println(eval(ti1))
  println(view(ti1))
}
