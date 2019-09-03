package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.dsl._
import xieyuheng.partech.example.collection._

object dec_sum {

  def dec_sum: Rule = Rule(
    "dec_sum", Map(
      "dec" -> Seq(dec),
      "dec_sum" -> Seq(dec_sum, " + ", dec_sum)))

  def dec = StrPred("dec", 1) { str => str.forall(Character.isDigit) }

  sealed trait DecSum
  final case class DecSumSum(x: DecSum, y: DecSum) extends DecSum
  final case class DecSumDec(n: Int) extends DecSum

  object DecSum {
    implicit def treeToDecSum: TreeTo[DecSum] = TreeTo[DecSum] { case tree =>
      tree match {
        case Node(Rule("dec_sum", _, _), "dec", Seq(Leaf(str))) =>
          DecSumDec(str.toInt)
        case Node(Rule("dec_sum", _, _), "dec_sum", Seq(x, Leaf(" + "), y)) =>
          DecSumSum(Tree.to[DecSum](x), Tree.to[DecSum](y))
        case _ => throw new Exception()
      }
    }
  }

}
