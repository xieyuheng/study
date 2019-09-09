package xieyuheng.partech

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

import xieyuheng.partech.ruleDSL._

object Earley {

  def init(words: List[Word], rule: Rule): Earley = {
    Earley(words: List[Word], rule: Rule)
  }

  case class Item(
    rule: Rule,
    choiceName: String,
    parts: List[RulePart],
    dot: Int,
    origin: Int,
    completedBy: Option[Item] = None,
  ) {
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
      completedBy match {
        case Some(causeOfCompletion) => s = s + s" { ${causeOfCompletion} }"
        case None => {}
      }
      s
    }

  }
}

case class Earley(words: List[Word], rule: Rule) {
  import Earley._

  val start = Rule("$Earley", Map("start" -> List(rule)))

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
          bringForwardItem(item, index)
        }
      case RulePartPred(pred) =>
        if (pred(words(index).str)) {
          val newParts = item.parts.patch(item.dot, List(RulePartStr(words(index).str)), 1)
          bringForwardItem(item.copy(parts = newParts), index)
        }
      case _ => {}
    }
  }

  def bringForwardItem(item: Item, index: Int): Unit = {
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

  def completeItem(causeOfCompletion: Item, index: Int): Unit = {
    val rule = causeOfCompletion.rule
    val itemset = active(causeOfCompletion.origin)
    itemset.foreach { case item =>
      item.parts(item.dot) match {
        case RulePartRule(ruleGen) =>
          if (rule == ruleGen()) {
            bringForwardItem(item.copy(completedBy = Some(causeOfCompletion)), index)
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
  }

  def report(): Unit = {
    val text = words.map(_.str).mkString(" ")

    println(s"text: ${text}")

    println()

    val indexes = 0 until words.length

    indexes.foreach { case index =>
      println(s"#${index}: ${words(index)}")
      println("- active:")
      active(index).foreach { case item => println(s"  ${item}") }
      println("- completed:")
      completed(index).foreach { case item => println(s"  ${item}") }
      println()
    }

    {
      val index = words.length

      println(s"#END:")
      println("- active:")
      active(index).foreach { case item => println(s"  ${item}") }
      println("- completed:")
      completed(index).foreach { case item => println(s"  ${item}") }
      println()
    }
  }

  run()

  val completedStarts: List[Item] = {
    val index = words.length
    completed(index).filter { case item =>
      item.rule == start &&
      item.dot == item.parts.length &&
      item.origin == 0
    }.toList
  }

  val recognize: Boolean = completedStarts.length > 0

  def nextTree(): Option[Tree] = {
    if (recognize) {
      val startItem = completedStarts(0)
      Some(collectNode(startItem))
    } else {
      None
    }
  }

  def collectNode(item: Item): Node = {
    item.completedBy match {
      case Some(causeOfCompletion) => {}
        val (newItem, children) = collectChildren(item)
        val itemset = active(causeOfCompletion.origin) ++ completed(causeOfCompletion.origin)
        val prevList = itemset.filter { case prev =>
          prev.dot == newItem.dot - 1 &&
          prev.rule == newItem.rule &&
          prev.choiceName == newItem.choiceName
        }.toList
        if (prevList.length == 0) {
            println(s"newItem: ${newItem}")
            println(s"causeOfCompletion: ${causeOfCompletion}")
            println(s"itemset:")
            itemset.foreach { case item => println(s"  ${item}")}
            throw new Exception()
        } else {
          if (prevList.length > 1) {
            println(s"many trees: ${prevList.length}")
            println(s"trees: ${prevList}")
            println(s"newItem: ${newItem}")
            println(s"causeOfCompletion: ${causeOfCompletion}")
            println(s"itemset:")
            itemset.foreach { case item => println(s"  ${item}")}
          }
          val prev = prevList(0)
          val node: Node = collectNode(prev)
          node.copy(children = (node.children :+ collectNode(causeOfCompletion)) ++ children)
        }
      case None =>
        val (_newItem, children) = collectChildren(item)
        Node(item.rule, item.choiceName, children)
    }
  }

  def collectChildren(item: Item): (Item, List[Tree]) = {

    import scala.annotation.tailrec

    @tailrec
    def countStrBeforeDot(n: Int, parts: List[RulePart], dot: Int): Int = {
      if (dot == 0) {
        n
      } else {
        parts(dot - 1) match {
          case RulePartStr(str) =>
            countStrBeforeDot(n + 1, parts, dot - 1)
          case RulePartRule(ruleGen) =>
            n
          case RulePartPred(pred) =>
            throw new Exception()
        }
      }
    }

    val n = countStrBeforeDot(0, item.parts, item.dot)

    val children: List[Tree] = item.parts.slice(item.dot - n, item.dot).map {
      case RulePartStr(str) => Leaf(str)
      case RulePartRule(ruleGen) =>
        throw new Exception()
      case RulePartPred(pred) =>
        throw new Exception()
    }

    (item.copy(dot = item.dot - n), children)
  }
}
