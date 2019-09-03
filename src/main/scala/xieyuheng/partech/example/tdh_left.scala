package xieyuheng.partech.example

import xieyuheng.partech._
import xieyuheng.partech.dsl._
import xieyuheng.partech.example.collection._

object tdh_left {

  // left regular grammar

  def tdh_left = Rule(
    "tdh_left", Map(
      "t" -> Seq("t"),
      "d" -> Seq("d"),
      "h" -> Seq("h"),
      "list" -> Seq(tdh_left_list)))

  def tdh_left_list = Rule(
    "tdh_left_list", Map(
      "t" -> Seq(tdh_left_list_head, "&t"),
      "d" -> Seq(tdh_left_list_head, "&d"),
      "h" -> Seq(tdh_left_list_head, "&h")))

  def tdh_left_list_head: Rule = Rule(
    "tdh_left_list_head", Map(
      "t" -> Seq("t"),
      "d" -> Seq("d"),
      "h" -> Seq("h"),
      "before_t" -> Seq(tdh_left_list_head, ",t"),
      "before_d" -> Seq(tdh_left_list_head, ",d"),
      "before_h" -> Seq(tdh_left_list_head, ",h")))

}