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
          List(treeToA(a))
        case Node(Rule("non_empty_list", _, _), "more", Seq(a, _, tail)) =>
          treeToA(a) :: tree_to_non_empty_list(treeToA)(tail)
        case _ => throw new Exception()
      }
  }


  def list(a: Rule)(implicit separater: String): Rule = Rule(
    s"list", Map(
      "null" -> Seq(),
      "non_empty" -> Seq(non_empty_list(a))),
    args = Map("a" -> a))

  implicit def tree_to_list[A]
    (implicit treeToA: TreeTo[A])
      : TreeTo[List[A]] = TreeTo[List[A]] { case tree =>
      tree match {
        case Node(Rule("list", _, _), "null", _) =>
          List()
        case Node(Rule("list", _, _), "non_empty", Seq(l)) =>
          tree_to_non_empty_list(treeToA)(l)
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


  def lower_case_letter_list: List[String] = List(
    "a", "b", "c", "d", "e", "f", "g",
    "h", "i", "j", "k", "l", "m", "n",
    "o", "p", "q", "r", "s", "t",
    "u", "v", "w", "x", "y", "z",
  )

  def lower_case_letter_gen(): String =
    lower_case_letter_list(
      rand.nextInt(
        lower_case_letter_list.length))

  def lower_case_letter = StrPred("lower_case_letter", 1, lower_case_letter_gen) {
    lower_case_letter_list.contains(_) }


  def upper_case_letter_list: List[String] = List(
    "A", "B", "C", "D", "E", "F", "G",
    "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T",
    "U", "V", "W", "X", "Y", "Z",
  )

  def upper_case_letter_gen(): String =
    upper_case_letter_list(
      rand.nextInt(
        upper_case_letter_list.length))

  def upper_case_letter = StrPred("upper_case_letter", 1, upper_case_letter_gen) {
    upper_case_letter_list.contains(_) }


  def non_space_char_gen(): String = {
    // only generating a subset of non_space_char
    val gen_list: List[() => String] = List(
      digit_char_gen,
      lower_case_letter_gen,
      upper_case_letter_gen,
    )
    val gen = gen_list(rand.nextInt(gen_list.length))
    gen()
  }

  def non_space_char = StrPred("non_space_char", 1, non_space_char_gen) {
    !space_char_list.contains(_) }

  def space = list(space_char)("")
}
