package xieyuheng.partech

import xieyuheng.partech.ruleDSL._

object predefined {

  def non_empty_list(a: Rule)(implicit separater: String): Rule = Rule(
    s"non_empty_list", Map(
      "one" -> Seq(a),
      "more" -> Seq(a, separater, non_empty_list(a))),
    args = Map("a" -> a))

  implicit def tree_to_non_empty_list[A]
    (implicit treeToA: TreeTo[A])
      : TreeTo[List[A]] = TreeTo[List[A]] { case tree =>
      tree match {
        case Node(Rule("non_empty_list", _, _), "one", Seq(a)) =>
          List(Tree.to[A](a))
        case Node(Rule("non_empty_list", _, _), "more", Seq(a, _, tail)) =>
          Tree.to[A](a) :: Tree.to[List[A]](tail)
        case _ => throw new Exception()
      }
  }

  val rand = scala.util.Random

  def space_char_list: List[String] = List("\n", "\t", " ")
  def space_char_gen(): String = space_char_list(rand.nextInt(space_char_list.length))
  def space_char = StrPred("space_char", 1, space_char_gen)(space_char_list.contains(_))

  def digit_char_list: List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  def digit_char_gen(): String = digit_char_list(rand.nextInt(digit_char_list.length))
  def digit_char = StrPred("digit_char", 1, digit_char_gen)(digit_char_list.contains(_))

  def space = ???

  def non_space_char = ???
}
