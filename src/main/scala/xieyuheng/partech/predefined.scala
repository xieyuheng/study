package xieyuheng.partech

import xieyuheng.partech.ruleDSL._

object predefined {

  def non_empty_list(a: Rule): Rule = Rule(
    "non_empty_list", Map(
      "one" -> List(a),
      "more" -> List(a, non_empty_list(a))),
    args = Map("a" -> a))

  def non_empty_list_matcher[A](matcher: Tree => A): Tree => List[A] = Tree.matcher(
    "non_empty_list", Map(
      "one" -> { case List(a) => List(matcher(a)) },
      "more" -> { case List(a, tail) => matcher(a) :: non_empty_list_matcher(matcher)(tail) }
    ))

  implicit def tree_to_non_empty_list[A]
    (implicit treeToA: TreeTo[A])
      : TreeTo[List[A]] = {
    TreeTo[List[A]](non_empty_list_matcher(treeToA.apply))
  }

  def wordInCharSet(set: Set[Char]): String => Boolean = {
    { case word => word.forall(set.contains(_)) }
  }


  def digit_char_set: Set[Char] = Set(
    '0', '1', '2', '3', '4',
    '5', '6', '7', '8', '9')

  def digit = wordInCharSet(digit_char_set)


  def lower_case_char_set: Set[Char] = Set(
    'a', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z',
  )

  def lower_case = wordInCharSet(lower_case_char_set)


  def upper_case_char_set: Set[Char] = Set(
    'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z',
  )

  def upper_case = wordInCharSet(upper_case_char_set)

}
