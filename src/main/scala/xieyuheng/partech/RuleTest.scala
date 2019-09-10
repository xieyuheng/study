package xieyuheng.partech

import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object RuleTest extends App {

  def lexer = Lexer.default

  def preservedIdentifiers = Set(
    "type", "case", "fn", "pi")

  def identifier = WordPred (
    "identifier", { case word =>
      if (preservedIdentifiers.contains(word)) {
        false
      } else {
        word.headOption match {
          case Some(char) =>
            val head_set = lower_case_char_set ++ upper_case_char_set + '_'
            val tail_set = head_set ++ digit_char_set
            head_set.contains(char) && wordInCharSet(tail_set)(word.tail)
          case None => false
        }
      }
    })

  def start = exp

  def exp: Rule = Rule(
    "exp", Map(
      "type" -> List("type"),
      "var" -> List(identifier),
      "case" -> List(exp, "case", "{", non_empty_list(case_clause), "}"),
      "dot" -> List(exp, ".", identifier),
      "pi" -> List("pi", "(", non_empty_list(arg), ")", ":", exp),
      "fn" -> List("fn", "(", non_empty_list(arg), ")", ":", exp, "=", exp),
      "ap" -> List(exp, "(", non_empty_list(arg), ")"),
    ))

  def case_clause = Rule.list(
    "case_clause", List(
      identifier, "=", ">", exp))

  case class Item(rule: Rule, choiceName: String, parts: List[RulePart], index: Int) {
    val matters = (rule, choiceName, parts.length, index)

    override def equals(that: Any): Boolean = {
      that match {
        case that: Item => this.matters == that.matters
        case _ => false
      }
    }

    override def hashCode = matters.hashCode

  }

  def arg = Rule(
    "arg", Map(
      "value" -> List(identifier, "=", exp),
      "type" ->  List(identifier, ":", exp),
      "value_comma" -> List(identifier, "=", exp, ","),
      "type_comma" ->  List(identifier, ":", exp, ","),
    ))

  def arg2 = Rule(
    "arg", Map(
      "value" -> List(identifier, "=", exp),
      "type" ->  List(identifier, ":", exp),
      "value_comma" -> List(identifier, "=", exp, ","),
      "type_comma" ->  List(identifier, ":", exp, ","),
    ))

  val item1 = Item(arg, "value", List(identifier, "=", exp), 0)
  val item2 = Item(arg2, "value", List(identifier, "=", exp), 0)

  assert(item1 == item2)

  import scala.collection.mutable.Set

  var itemSet: Set[Item] = Set()
  itemSet += item1
  itemSet += item2

  assert(itemSet.size == 1)
}
