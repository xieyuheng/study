package xieyuheng.partech

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

object Earley {

  def init(words: List[Word], rule: Rule): Earley = {
    Earley(words: List[Word], rule: Rule)
  }

  case class Item(rule: Rule, choiceName: String, parts: List[RulePart], dot: Int, origin: Int) {
    val matters = (rule, choiceName, parts.length, dot, origin)

    override def equals(that: Any): Boolean = {
      that match {
        case that: Item => this.matters == that.matters
        case _ => false
      }
    }

    override def hashCode = matters.hashCode

    override def toString(): String = {
      var s = s"${rule.name}:${choiceName} -> "
      val indexes = 0 until parts.length
      indexes.foreach { case index =>
        val part = parts(index)
        if (dot == index) {
          s = s + "• "
        }
        s = s + s"${part} "
      }
      if (dot == parts.length) {
        s = s + "• "
      }
      s = s + s"[${origin}]"
      s
    }

  }
}

case class Earley(words: List[Word], start: Rule) {
  import Earley._

  var active: ArrayBuffer[Set[Item]] = ArrayBuffer.fill(words.length + 1)(Set())
  var completed: ArrayBuffer[Set[Item]] = ArrayBuffer.fill(words.length + 1)(Set())

  def predict(index: Int): Unit = {
    val itemset = active(index)

    var beforeSize: Option[Int] = Some(itemset.size)
    var afterSize: Option[Int] = None

    while (beforeSize != afterSize) {
      beforeSize = Some(itemset.size)
      itemset.foreach { case item =>
        predictItem(item, index)
      }
      afterSize = Some(itemset.size)
    }
  }

  def predictItem(item: Item, index: Int): Unit = {
    item.parts(item.dot) match {
      case RulePartRule(ruleGen) =>
        val rule = ruleGen()
        expendRule(rule, index)
      case _ => {}
    }
  }

  def expendRule(rule: Rule, index: Int): Unit = {
    rule.choices.foreach { case (choiceName, parts) =>
      active(index) += Item(rule, choiceName, parts, 0, index)
    }
  }

  def scan(index: Int): Unit = {
    active(index).foreach { case item =>
      scanItem(item, index)
    }
  }

  def scanItem(item: Item, index: Int): Unit = {
    item.parts(item.dot) match {
      case RulePartStr(str) =>
        if (str == words(index).str) {
          putItem(item, index)
        }
      case RulePartPred(pred) =>
        if (pred(words(index).str)) {
          putItem(item, index)
        }
      case _ => {}
    }
  }

  def putItem(item: Item, index: Int): Unit = {
    val itemset = if (item.dot + 1 == item.parts.length) {
      completed(index + 1)
    } else {
      active(index + 1)
    }
    itemset += item.copy(dot = item.dot + 1)
  }

  def complete(index: Int): Unit = {
    val itemset = completed(index + 1)

    var beforeSize: Option[Int] = Some(itemset.size)
    var afterSize: Option[Int] = None

    while (beforeSize != afterSize) {
      beforeSize = Some(itemset.size)
      itemset.foreach { case item =>
        completeItem(item, index)
      }
      afterSize = Some(itemset.size)
    }
  }

  def completeItem(item: Item, index: Int): Unit = {
    val rule = item.rule
    val itemset = active(item.origin)
    itemset.foreach { case item =>
      item.parts(item.dot) match {
        case RulePartRule(ruleGen) =>
          if (rule == ruleGen()) {
            putItem(item, index)
          }
        case _ => {}
      }
    }
  }

  def run(): Unit = {
    expendRule(start, 0)
    val indexes = 0 until words.length
    indexes.foreach { case index =>
      predict(index)
      scan(index)
      complete(index)
    }
    report()
  }

  def report(): Unit = {
    val indexes = 0 until words.length

    indexes.foreach { case index =>
      println(s"#${index}: ${words(index)}")
      println("active:")
      active(index).foreach { case item => println(s"  ${item}") }
      println("completed:")
      completed(index).foreach { case item => println(s"  ${item}") }
      println()
    }

    {
      val index = words.length

      println(s"THE END")
      println("active:")
      active(index).foreach { case item => println(s"  ${item}") }
      println("completed:")
      completed(index).foreach { case item => println(s"  ${item}") }
      println()
    }
  }

  // val recognize: Boolean = {
  //   ???
  // }

  // def nextTree(): Option[Tree] = {
  //   ???
  // }
}
