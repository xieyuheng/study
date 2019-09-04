package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object abc extends ExampleRule {

  // ambiguous grammar

  // language = "a"^m "b"^n "c"^n | "a"^p "b"^p "c"^q

  val sentences = Seq(
    "abc",
    "aabbcc",
  )

  val non_sentences = Seq(
    "abbccc",
  )

  def main = abc

  def treeToMainType = Some(ABC.treeToABC)

  def abc = Rule(
    "abc", Map(
      "a_bc" -> Seq(a, bc),
      "ab_c" -> Seq(ab, c)))

  def a: Rule = Rule(
    "a", Map(
      "one_a" -> Seq("a"),
      "more_a" -> Seq("a", a)))

  def ab: Rule = Rule(
    "ab", Map(
      "one_ab" -> Seq("ab"),
      "more_ab" -> Seq("a", ab, "b")))

  def bc: Rule = Rule(
    "bc", Map(
      "one_bc" -> Seq("bc"),
      "more_bc" -> Seq("b", bc, "c")))

  def c: Rule = Rule(
    "c", Map(
      "one_c" -> Seq("c"),
      "more_c" -> Seq("c", c)))

  sealed trait ABC
  final case class A_BC(a: A, bc: BC) extends ABC
  final case class AB_C(ab: AB, c: C) extends ABC

  object ABC {
    implicit def treeToABC: TreeTo[ABC] = TreeTo[ABC] { case tree =>
      tree match {
        case Node(Rule("abc", _, _), "a_bc", Seq(a, bc)) =>
          A_BC(Tree.to[A](a), Tree.to[BC](bc))
        case Node(Rule("abc", _, _), "ab_c", Seq(ab, c)) =>
          AB_C(Tree.to[AB](ab), Tree.to[C](c))
        case _ => throw new Exception()
      }
    }
  }

  sealed trait A
  final case class ONE_A() extends A
  final case class MORE_A(a: A) extends A

  object A {
    implicit def treeToA: TreeTo[A] = TreeTo[A] { case tree =>
      tree match {
        case Node(Rule("a", _, _), "one_a", _) =>
          ONE_A()
        case Node(Rule("a", _, _), "more_a", Seq(_, a)) =>
          MORE_A(Tree.to[A](a))
        case _ => throw new Exception()
      }
    }
  }

  sealed trait AB
  final case class ONE_AB() extends AB
  final case class MORE_AB(ab: AB) extends AB

  object AB {
    implicit def treeToAB: TreeTo[AB] = TreeTo[AB] { case tree =>
      tree match {
        case Node(Rule("ab", _, _), "one_ab", _) =>
          ONE_AB()
        case Node(Rule("ab", _, _), "more_ab", Seq(_, ab, _)) =>
          MORE_AB(Tree.to[AB](ab))
        case _ => throw new Exception()
      }
    }
  }

  sealed trait BC
  final case class ONE_BC() extends BC
  final case class MORE_BC(bc: BC) extends BC

  object BC {
    implicit def treeToBC: TreeTo[BC] = TreeTo[BC] { case tree =>
      tree match {
        case Node(Rule("bc", _, _), "one_bc", _) =>
          ONE_BC()
        case Node(Rule("bc", _, _), "more_bc", Seq(_, bc, _)) =>
          MORE_BC(Tree.to[BC](bc))
        case _ => throw new Exception()
      }
    }
  }

  sealed trait C
  final case class ONE_C() extends C
  final case class MORE_C(c: C) extends C

  object C {
    implicit def treeToC: TreeTo[C] = TreeTo[C] { case tree =>
      tree match {
        case Node(Rule("c", _, _), "one_c", _) =>
          ONE_C()
        case Node(Rule("c", _, _), "more_c", Seq(_, c)) =>
          MORE_C(Tree.to[C](c))
        case _ => throw new Exception()
      }
    }
  }

}
