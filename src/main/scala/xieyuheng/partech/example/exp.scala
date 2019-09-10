package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.ruleDSL._
import xieyuheng.partech.predefined._

object exp extends ExampleRule {

  def lexer = Lexer.default

  def sentences = List(
    "type",
    "n",
    "x.prev",
    "succ_t(prev = nat_add(x = x.prev, y = y))",

    """
    x case {
      zero_t => y
      succ_t => succ_t(prev = nat_add(x = x.prev, y = y))
    }
    """,

    """
    x case {
      zero_t => y
      succ_t => succ_t(prev = nat_add(x = x.prev, y = x case {
        zero_t => y
        succ_t => succ_t(prev = nat_add(x = x.prev, y = y))
      }))
    }
    """,

    """
    x case {
      zero_t => x case {
         zero_t => y
         succ_t => succ_t(prev = nat_add(x = x.prev, y = x case {
           zero_t => y
           succ_t => succ_t(prev = nat_add(x = x.prev, y = y))
         }))
       }
      succ_t => succ_t(prev = nat_add(x = x.prev, y = x case {
        zero_t => y
        succ_t => succ_t(prev = nat_add(x = x.prev, y = y))
      }))
    }
    """,
  )

  def non_sentences = List(
    "x..y",
    "x...y",
  )

  def preservedIdentifiers = Set(
    "type", "case", "fn", "pi")

  def identifier = WordPred(
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

  def treeToMainType = None

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

  def arg = Rule(
    "arg", Map(
      "value" -> List(identifier, "=", exp),
      "type" ->  List(identifier, ":", exp),
      "value_comma" -> List(identifier, "=", exp, ","),
      "type_comma" ->  List(identifier, ":", exp, ","),
    ))

}
