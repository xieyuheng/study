package xieyuheng.partech

import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

import xieyuheng.partech.ruleDSL._

// Earley parser -- O(n^3)
// - collect parse tree by Earley's method, can not handle ambiguity.
// - ambiguity check is not decidable, thus only reported at runtime.
// - can not handle epsilon

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
      case RulePartPred(wordPred) =>
        if (wordPred.pred(words(index).str)) {
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

  def parse(): Either[ErrMsg, Tree] = {
    if (completedStarts.length == 0) {
      Left(ErrMsg("Earley.parse",
        s"fail to recognize",
        Span(0, 0)))
    } else if (completedStarts.length > 1) {
      Left(ErrMsg("Earley.parse",
        s"grammar is ambiguous, found ${completedStarts.length} parse trees",
        Span(0, 0)))
    } else {
      val startItem = completedStarts(0)
      collectNode(startItem).flatMap { case tree =>
        if (tree.children.length != 1) {
          Left(ErrMsg("Earley.parse",
            s"collected multiple nodes under start, number of nodes: ${tree.children.length}",
            Span(0, 0)))
        } else {
          Right(tree.children(0))
        }
      }
    }
  }

  def collectNode(item: Item): Either[ErrMsg, Node] = {
    item.completedBy match {
      case Some(causeOfCompletion) =>
        for {
          pair <- collectChildren(item)
          (newItem, children) = pair

          itemset = active(causeOfCompletion.origin) ++ completed(causeOfCompletion.origin)

          prevList = itemset.filter { case prev =>
            prev.dot == newItem.dot - 1 &&
            prev.rule == newItem.rule &&
            prev.choiceName == newItem.choiceName
          }.toList

          result <- {
            if (prevList.length == 0) {
              val msg = {
                s"newItem: ${newItem}" ::
                s"causeOfCompletion: ${causeOfCompletion}" ::
                s"itemset:" ::
                itemset.map { case item => s"  ${item}"}.toList
              }.mkString("\n")
              Left(ErrMsg("Earley.parse", msg, Span(0, 0)))
            } else {
              if (prevList.length > 1) {
                val msg = {
                  s"many trees: ${prevList.length}" ::
                  s"prevList:" ::
                  prevList.map { case item => s"  ${item}"} ++
                  s"newItem: ${newItem}" ::
                  s"causeOfCompletion: ${causeOfCompletion}" ::
                  s"itemset:" ::
                  itemset.map { case item => s"  ${item}"}.toList
                }.mkString("\n")
                Left(ErrMsg("Earley.parse", msg, Span(0, 0)))
              } else {
                val prev = prevList(0)
                for {
                  node <- collectNode(prev)
                  mid <- collectNode(causeOfCompletion)
                } yield node.copy(
                  children = (node.children :+ mid) ++ children)
              }
            }
          }

        } yield result

      case None =>
        for {
          pair <- collectChildren(item)
          (_newItem, children) = pair
        } yield Node(item.rule, item.choiceName, children)
    }
  }

  def collectChildren(item: Item): Either[ErrMsg, (Item, List[Tree])] = {
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
          case RulePartPred(wordPred) =>
            throw new Exception()
        }
      }
    }

    val n = countStrBeforeDot(0, item.parts, item.dot)

    val children: List[Tree] = item.parts.slice(item.dot - n, item.dot).map {
      case RulePartStr(str) => Leaf(str)
      case RulePartRule(ruleGen) =>
        throw new Exception()
      case RulePartPred(wordPred) =>
        throw new Exception()
    }

    Right((item.copy(dot = item.dot - n), children))
  }
}
