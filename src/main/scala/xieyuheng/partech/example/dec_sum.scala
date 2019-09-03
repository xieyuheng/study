package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.example.collection._

object dec_sum extends ExampleRule {

  val sentences = Seq(
    "1 + 0",
    "1 + 1 + 1 + 0",
    "0 + 0",
    "1 + 2",
    "1 + 9",
    "1 + 9 + 5",
  )

  val non_sentences = Seq(
    "11 + 12",
  )

  def main = dec_sum

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
