package xieyuheng.partech.lexerless.example

import xieyuheng.partech.lexerless._
import xieyuheng.partech.lexerless.ruleDSL._
import xieyuheng.partech.lexerless.predefined._

object tdh_left extends ExampleRule {

  // left regular grammar

  val sentences = List(
    "t,d&h",
  )

  val non_sentences = List(
    "t,d,h",
  )

  def start = tdh_left

  def treeToMainType = None

  def tdh_left = Rule(
    "tdh_left", Map(
      "t" -> List("t"),
      "d" -> List("d"),
      "h" -> List("h"),
      "list" -> List(tdh_left_list)))

  def tdh_left_list = Rule(
    "tdh_left_list", Map(
      "t" -> List(tdh_left_list_head, "&t"),
      "d" -> List(tdh_left_list_head, "&d"),
      "h" -> List(tdh_left_list_head, "&h")))

  def tdh_left_list_head: Rule = Rule(
    "tdh_left_list_head", Map(
      "t" -> List("t"),
      "d" -> List("d"),
      "h" -> List("h"),
      "before_t" -> List(tdh_left_list_head, ",t"),
      "before_d" -> List(tdh_left_list_head, ",d"),
      "before_h" -> List(tdh_left_list_head, ",h")))

}
