package xieyuheng.partech

import xieyuheng.partech.dsl._

object RuleExample {

  def non_empty_list(a: Rule)(implicit separater: String): Rule = Rule(
    s"list(${a.name})", Map(
      "unit" -> Seq(a),
      "cons" -> Seq(a, separater, non_empty_list(a))))

  implicit def non_empty_list_from_tree[A](implicit aFromTree: FromTree[A]) = {
    new FromTree[List[A]] {
      def fromTree(tree: Tree): List[A] = {
        tree match {
          case Node(_, "unit", rule, Seq(a)) =>
            List(aFromTree(a))
          case Node(_, "cons", rule, Seq(a, _, tail)) =>
            aFromTree(a) :: fromTree(tail)
          case _ => throw new Exception()
        }
      }
    }
  }
  
  object bool_sexp {
    def bool = Rule(
      "bool", Map(
        "true" -> Seq("true"),
        "false" -> Seq("false")))

    sealed trait Bool
    final case object True extends Bool
    final case object False extends Bool

    object Bool {
      implicit object BoolFromTree extends FromTree[Bool] {
        def fromTree(tree: Tree): Bool = {
          tree match {
            case Node("bool", "true", rule, Seq(Leaf("true"))) =>
              True
            case Node("bool", "false", rule, Seq(Leaf("false"))) =>
              False
            case _ => throw new Exception()
          }
        }
      }
    }

    def bool_sexp = Rule(
      "bool_sexp", Map(
        "list" -> Seq("(", bool_sexp_list, ")"),
        "bool" -> Seq(bool)))

    sealed trait BoolSexp
    final case class BoolSexpList(list: List[BoolSexp]) extends BoolSexp
    final case class BoolSexpBool(bool: Bool) extends BoolSexp

    object BoolSexp {
      implicit object BoolSexpFromTree extends FromTree[BoolSexp] {
        def fromTree(tree: Tree): BoolSexp = {
          tree match {
            case Node("bool_sexp", "list", rule, Seq(_, list, _)) =>
              BoolSexpList(Tree.to[List[BoolSexp]](list))
            case Node("bool_sexp", "bool", rule, Seq(bool)) =>
              BoolSexpBool(Tree.to[Bool](bool))
            case _ => throw new Exception()
          }
        }
      }
    }

    // def bool_sexp_list: Rule = Rule(
    //   "bool_sexp_list", Map(
    //     "unit" -> Seq(bool_sexp),
    //     "cons" -> Seq(bool_sexp, " ", bool_sexp_list)))

    def bool_sexp_list: Rule = non_empty_list(bool_sexp)(" ")
  }

  object tom_dick_and_harry {
    def tom_dick_and_harry = Rule(
      "tom_dick_and_harry", Map(
        "name" -> Seq(name),
        "list" -> Seq(name_list, " and ", name)))

    def name = Rule(
      "name", Map(
        "tom" -> Seq("tom"),
        "dick" -> Seq("dick"),
        "harry" -> Seq("harry")))

    // def name_list: Rule = Rule(
    //   "name_list", Map(
    //     "unit" -> Seq(name),
    //     "cons" -> Seq(name, ", ", name_list)))

    def name_list = non_empty_list(name)(", ")
  }

  object tdh {
    // regular grammar

    def tdh = Rule(
      "tdh", Map(
        "t" -> Seq("t"),
        "d" -> Seq("d"),
        "h" -> Seq("h"),
        "tdh_list" -> Seq(tdh_list)))

    def tdh_list = Rule(
      "tdh_list", Map(
        "t" -> Seq("t", tdh_list_tail),
        "d" -> Seq("d", tdh_list_tail),
        "h" -> Seq("h", tdh_list_tail)))

    def tdh_list_tail: Rule = Rule(
      "tdh_list_tail", Map(
        "list" -> Seq(",", tdh_list),
        "t" -> Seq("&t"),
        "d" -> Seq("&d"),
        "h" -> Seq("&h")))
  }

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

  object sum {
    def sum: Rule = Rule(
      "sum", Map(
        "digit" -> Seq(digit),
        "sum" -> Seq(sum, " + ", sum)))

    def digit = Rule(
      "digit", Map(
        "0" -> Seq("0"),
        "1" -> Seq("1")))

    sealed trait Sum
    final case class SumSum(x: Sum, y: Sum) extends Sum
    final case class DigitSum(n: Int) extends Sum

    object Sum {
      implicit object SumFromTree extends FromTree[Sum] {
        def fromTree(tree: Tree): Sum = {
          tree match {
            case Node("sum", "digit", rule, Seq(Node("digit", "0", _, _))) =>
              DigitSum(0)
            case Node("sum", "digit", rule, Seq(Node("digit", "1", _, _))) =>
              DigitSum(1)
            case Node("sum", "sum", rule, Seq(x, Leaf(" + "), y)) =>
              SumSum(fromTree(x), fromTree(y))
            case _ => throw new Exception()
          }
        }
      }
    }
  }

  object ab {
    // equal number of "a" "b"

    def ab = Rule(
      "ab", Map(
        "head_a" -> Seq("a", b),
        "head_b" -> Seq("b", a)))

    def a: Rule = Rule(
      "a", Map(
        "one" -> Seq("a"),
        "more" -> Seq("a", ab),
        "after_b" -> Seq("b", a, a)))

    def b: Rule = Rule(
      "b", Map(
        "one" -> Seq("b"),
        "more" -> Seq("b", ab),
        "after_a" -> Seq("a", b, b)))
  }

  object abc {
    // ambiguous grammar
    // language = "a"^m "b"^n "c"^n | "a"^p "b"^p "c"^q

    def abc = Rule(
      "abc", Map(
        "a_bc" -> Seq(a, bc),
        "ab_c" -> Seq(ab, c)))

    def a: Rule = Rule(
      "a", Map(
        "one" -> Seq("a"),
        "more" -> Seq("a", a)))

    def bc: Rule = Rule(
      "bc", Map(
        "one" -> Seq("bc"),
        "more" -> Seq("b", bc, "c")))

    def ab: Rule = Rule(
      "ab", Map(
        "one" -> Seq("ab"),
        "more" -> Seq("a", ab, "b")))

    def c: Rule = Rule(
      "c", Map(
        "one" -> Seq("c"),
        "more" -> Seq("c", c)))
  }
}
