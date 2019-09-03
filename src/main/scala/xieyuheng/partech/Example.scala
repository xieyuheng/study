package xieyuheng.partech

import xieyuheng.partech.dsl._

object Example {

  def non_empty_list(a: Rule)(implicit separater: String): Rule = Rule(
    s"non_empty_list", Map(
      "one" -> Seq(a),
      "more" -> Seq(a, separater, non_empty_list(a))),
    args = Map("a" -> a))

  implicit def tree_to_non_empty_list[A]
    (implicit treeToA: TreeTo[A])
      : TreeTo[List[A]] = TreeTo[List[A]] { case tree =>
      tree match {
        case Node(Rule("non_empty_list", _, _), "one", Seq(a)) =>
          List(Tree.to[A](a))
        case Node(Rule("non_empty_list", _, _), "more", Seq(a, _, tail)) =>
          Tree.to[A](a) :: Tree.to[List[A]](tail)
        case _ => throw new Exception()
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
      implicit def treeToBool = TreeTo[Bool] { case tree =>
        tree match {
          case Node(Rule("bool", _, _), "true", Seq(Leaf("true"))) =>
            True
          case Node(Rule("bool", _, _), "false", Seq(Leaf("false"))) =>
            False
          case _ => throw new Exception()
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
      implicit def treeToBoolSexp: TreeTo[BoolSexp] = TreeTo[BoolSexp] { case tree =>
        tree match {
          case Node(Rule("bool_sexp", _, _), "list", Seq(_, list, _)) =>
            BoolSexpList(Tree.to[List[BoolSexp]](list))
          case Node(Rule("bool_sexp", _, _), "bool", Seq(bool)) =>
            BoolSexpBool(Tree.to[Bool](bool))
          case _ => throw new Exception()
        }
      }
    }

    // def bool_sexp_list: Rule = Rule(
    //   "bool_sexp_list", Map(
    //     "one" -> Seq(bool_sexp),
    //     "more" -> Seq(bool_sexp, " ", bool_sexp_list)))

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
    //     "one" -> Seq(name),
    //     "more" -> Seq(name, ", ", name_list)))

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

  object dec_sum {
    def dec_sum: Rule = Rule(
      "dec_sum", Map(
        "dec" -> Seq(dec),
        "dec_sum" -> Seq(dec_sum, " + ", dec_sum)))

    def dec = StrPred(1, str => str.forall(Character.isDigit), "dec")

    sealed trait DecSum
    final case class DecSumSum(x: DecSum, y: DecSum) extends DecSum
    final case class DecSumDec(n: Int) extends DecSum

    object DecSum {
      implicit def treeToDecSum: TreeTo[DecSum] = TreeTo[DecSum] { case tree =>
        tree match {
          case Node(Rule("dec_sum", _, _), "dec", Seq(Leaf(str))) =>
            DecSumDec(str.toInt)
          case Node(Rule("dec_sum", _, _), "dec_sum", Seq(x, Leaf(" + "), y)) =>
            DecSumSum(Tree.to[DecSum](x), Tree.to[DecSum](y))
          case _ => throw new Exception()
        }
      }
    }
  }

  object bin_sum {
    def bin_sum: Rule = Rule(
      "bin_sum", Map(
        "bin" -> Seq(bin),
        "bin_sum" -> Seq(bin_sum, " + ", bin_sum)))

    def bin = Rule(
      "bin", Map(
        "0" -> Seq("0"),
        "1" -> Seq("1")))

    sealed trait BinSum
    final case class BinSumSum(x: BinSum, y: BinSum) extends BinSum
    final case class BinSumBin(n: Int) extends BinSum

    object BinSum {
      implicit def treeToBinSum: TreeTo[BinSum] = TreeTo[BinSum] { case tree =>
        tree match {
          case Node(Rule("bin_sum", _, _), "bin", Seq(Node(Rule("bin", _, _), "0", _))) =>
            BinSumBin(0)
          case Node(Rule("bin_sum", _, _), "bin", Seq(Node(Rule("bin", _, _), "1", _))) =>
            BinSumBin(1)
          case Node(Rule("bin_sum", _, _), "bin_sum", Seq(x, Leaf(" + "), y)) =>
            BinSumSum(Tree.to[BinSum](x), Tree.to[BinSum](y))
          case _ => throw new Exception()
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
