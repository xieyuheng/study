package xieyuheng.partech

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class LinearParseTree(parts: List[LinearParsePart]) {
  def indexOfNextVar(): Int = {
    parts.indexWhere { case part =>
      part.isInstanceOf[LinearParseVar]
    }
  }

  def complete(): Boolean = {
    indexOfNextVar == -1
  }

  def expend(): List[LinearParseTree] = {
    indexOfNextVar match {
      case -1 => List()
      case n => {
        val LinearParseVar(rule) = parts(n)
        rule.choices.map { case (choiceName, ruleParts) =>
          val newParts = {
            List(LinearParseBra(rule, choiceName)) ++
            ruleParts.map(LinearParsePart.fromRulePart) ++
            List(LinearParseKet(rule, choiceName))
          }
          LinearParseTree(parts.patch(n, newParts, 1))
        } .toList
      }
    }
  }

  def toXML(): String = {
    parts.map { case part =>
      part match {
        case LinearParseStr(str) => '"' + str + '"'
        case LinearParseVar(rule) => "<" ++ rule.name ++ ">"
        case LinearParseBra(rule, choiceName) => "<" ++ rule.name ++ ":" ++ choiceName ++ ">"
        case LinearParseKet(rule, choiceName) => "<" ++ rule.name ++ ":" ++ choiceName ++ ">"
      }
    }
    .mkString(" ")
  }
}

object LinearParseTree {
  def fromRule(rule: Rule): LinearParseTree = {
    LinearParseTree(List(LinearParseVar(rule)))
  }
}

sealed trait LinearParsePart
final case class LinearParseStr(str: String) extends LinearParsePart
final case class LinearParseVar(rule: Rule) extends LinearParsePart
final case class LinearParseBra(rule: Rule, choiceName: String) extends LinearParsePart
final case class LinearParseKet(rule: Rule, choiceName: String) extends LinearParsePart

object LinearParsePart {
  def fromRulePart(rulePart: RulePart): LinearParsePart = {
    rulePart match {
      case RulePartStr(str) => LinearParseStr(str)
      case RulePartRule(ruleGen) => {
        val rule = ruleGen()
        LinearParseVar(rule)
      }
    }
  }
}

case class Generator(
  queue: ListBuffer[LinearParseTree] = ListBuffer(),
) {

  def nextLinearParseTree(): Option[LinearParseTree] = {
    var result: Option[LinearParseTree] = None

    breakable {
      while (true) {
        queue.headOption match {
          case Some(tree) => {
            if (tree.complete()) {
              queue.trimStart(1)
              result = Some(tree)
              break
            } else {
              queue.trimStart(1)
              queue.appendAll(tree.expend())
            }
          }
          case None => break
        }
      }
    }

    result
  }

  def take(n: Int): List[LinearParseTree] = {
    var count: Int = 0
    var list: List[LinearParseTree] = List()

    while (count < n) {
      count += 1
      nextLinearParseTree() match {
        case Some(tree) => list = list ++ List(tree)
        case None => {}
      }
    }

    list
  }
}

object Generator {
  def fromRule(rule: Rule): Generator = {
    val queue: ListBuffer[LinearParseTree] = ListBuffer(LinearParseTree.fromRule(rule))
    Generator(queue)
  }
}
