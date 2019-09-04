package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.example.collection._

object ab extends ExampleRule {

  // equal number of "a" "b"

  val sentences = Seq(
    "ab",
    "abab",
    "aabb",
  )

  val non_sentences = Seq(
    "aab",
  )

  def main = ab

  def treeToMainType = Some(AB.treeToAB)

  def ab = Rule(
    "ab", Map(
      "head_a" -> Seq("a", b),
      "head_b" -> Seq("b", a)))

  def a: Rule = Rule(
    "a", Map(
      "one_a" -> Seq("a"),
      "more_a" -> Seq("a", ab),
      "after_b" -> Seq("b", a, a)))

  def b: Rule = Rule(
    "b", Map(
      "one_b" -> Seq("b"),
      "more_b" -> Seq("b", ab),
      "after_a" -> Seq("a", b, b)))

  sealed trait AB
  final case class HEAD_A(b: B) extends AB
  final case class HEAD_B(a: A) extends AB

  object AB {
    implicit def treeToAB: TreeTo[AB] = TreeTo[AB] { case tree =>
      tree match {
        case Node(Rule("ab", _, _), "head_a", Seq(_, b)) =>
          HEAD_A(Tree.to[B](b))
        case Node(Rule("ab", _, _), "head_b", Seq(_, a)) =>
          HEAD_B(Tree.to[A](a))
        case _ => throw new Exception()
      }
    }
  }

  sealed trait A
  final case class ONE_A() extends A
  final case class MORE_A(ab: AB) extends A
  final case class AFTER_B(a1: A, a2: A) extends A

  object A {
    implicit def treeToA: TreeTo[A] = TreeTo[A] { case tree =>
      tree match {
        case Node(Rule("a", _, _), "one_a", _) =>
          ONE_A()
        case Node(Rule("a", _, _), "more_a", Seq(_, ab)) =>
          MORE_A(Tree.to[AB](ab))
        case Node(Rule("a", _, _), "after_b", Seq(_, a1, a2)) =>
          AFTER_B(Tree.to[A](a1), Tree.to[A](a2))
        case _ => throw new Exception()
      }
    }
  }

  sealed trait B
  final case class ONE_B() extends B
  final case class MORE_B(ab: AB) extends B
  final case class AFTER_A(b1: B, b2: B) extends B

  object B {
    implicit def treeToB: TreeTo[B] = TreeTo[B] { case tree =>
      tree match {
        case Node(Rule("b", _, _), "one_b", _) =>
          ONE_B()
        case Node(Rule("b", _, _), "more_b", Seq(_, ab)) =>
          MORE_B(Tree.to[AB](ab))
        case Node(Rule("b", _, _), "after_a", Seq(_, b1, b2)) =>
          AFTER_A(Tree.to[B](b1), Tree.to[B](b2))
        case _ => throw new Exception()
      }
    }
  }

}
