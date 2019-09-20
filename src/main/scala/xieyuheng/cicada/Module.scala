package xieyuheng.cicada

// import check._
import pretty._

case class Module() {

  var top_list: List[Top] = List()

  def add_top(top: Top): Module = {
    top_list = top_list :+ top
    this
  }

  def declare(decl: Decl): Unit = {
    add_top(TopDecl(decl))
  }

  def run(): Unit = {
    top_list.map {
      case TopDecl(decl: Decl) =>
        s"${pretty_decl(decl)}"
      case TopEval(exp: Exp) =>
        s"eval! ${pretty_exp(exp)}"
      case TopEq(x: Exp, y: Exp) =>
        s"eq! ${pretty_exp(x)} ${pretty_exp(y)}"
      case TopNotEq(x: Exp, y: Exp) =>
        s"not_eq! ${pretty_exp(x)} ${pretty_exp(y)}"
    }.foreach {
      case str =>
        println(str)
        println()
    }
  }

}
