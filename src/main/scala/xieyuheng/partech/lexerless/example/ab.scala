package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object ab extends ExampleRule {

  // equal number of "a" "b"

  val sentences = List(
    "ab",
    "abab",
    "aabb",
  )

  val non_sentences = List(
    "aab",
  )

  def start = ab

  def treeToMainType = Some(AB.treeToAB)

  def ab = Rule(
    "ab", Map(
      "head_a" -> List("a", b),
      "head_b" -> List("b", a)))

  def a: Rule = Rule(
    "a", Map(
      "one_a" -> List("a"),
      "more_a" -> List("a", ab),
      "after_b" -> List("b", a, a)))

  def b: Rule = Rule(
    "b", Map(
      "one_b" -> List("b"),
      "more_b" -> List("b", ab),
      "after_a" -> List("a", b, b)))

  sealed trait AB
  final case class HEAD_A(b: B) extends AB
  final case class HEAD_B(a: A) extends AB

  object AB {
    implicit def treeToAB: TreeTo[AB] = TreeTo[AB] { case tree =>
      tree match {
        case Node(Rule("ab", _, _), "head_a", List(_, b)) =>
          HEAD_A(Tree.to[B](b))
        case Node(Rule("ab", _, _), "head_b", List(_, a)) =>
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
        case Node(Rule("a", _, _), "more_a", List(_, ab)) =>
          MORE_A(Tree.to[AB](ab))
        case Node(Rule("a", _, _), "after_b", List(_, a1, a2)) =>
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
        case Node(Rule("b", _, _), "more_b", List(_, ab)) =>
          MORE_B(Tree.to[AB](ab))
        case Node(Rule("b", _, _), "after_a", List(_, b1, b2)) =>
          AFTER_A(Tree.to[B](b1), Tree.to[B](b2))
        case _ => throw new Exception()
      }
    }
  }

}
