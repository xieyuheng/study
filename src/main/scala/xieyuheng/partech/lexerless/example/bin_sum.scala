package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object bin_sum extends ExampleRule {

  val sentences = Seq(
    "1 + 0",
    "1 + 1 + 1 + 0",
    "0 + 0",
  )

  val non_sentences = Seq(
    "1 + 2",
  )

  def main = bin_sum

  def treeToMainType = Some(BinSum.treeToBinSum)

  def bin_sum: Rule = Rule(
    "bin_sum", Map(
      "bin" -> Seq(bin),
      "bin_sum" -> Seq(bin_sum, " + ", bin_sum)))

  def bin = Rule(
    "bin", Map(
      "0" -> Seq("0"),
      "1" -> Seq("1")))

  sealed trait BinSum
  final case class BinSumSum(x: BinSum, y: BinSum) extends BinSum
  final case class BinSumBin(n: Int) extends BinSum

  object BinSum {
    implicit def treeToBinSum: TreeTo[BinSum] = TreeTo[BinSum] { case tree =>
      tree match {
        case Node(Rule("bin_sum", _, _), "bin", Seq(Node(Rule("bin", _, _), "0", _))) =>
          BinSumBin(0)
        case Node(Rule("bin_sum", _, _), "bin", Seq(Node(Rule("bin", _, _), "1", _))) =>
          BinSumBin(1)
        case Node(Rule("bin_sum", _, _), "bin_sum", Seq(x, Leaf(" + "), y)) =>
          BinSumSum(Tree.to[BinSum](x), Tree.to[BinSum](y))
        case _ => throw new Exception()
      }
    }
  }
}
