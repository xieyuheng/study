package xieyuheng.partech

object ParserTest extends App {
  import xieyuheng.partech.dsl._
  import RuleExample._

  def show(rule: Rule, text: String): Unit = {
    Parser(rule).parsing(text).nextTree match {
      case Some(tree) => println(pretty.prettyTree(tree))
      case None => println(s"[PARSING ERROR] rule: ${rule.name}, text: ${text}")
    }
  }

  Seq(
    "(true false)",
    "(true false true)",
    "(true ((((false)))))",
  ).foreach { show(bool_sexp.bool_sexp, _) }

  Seq(
    "tom, dick and harry",
  ).foreach { show(tom_dick_and_harry.tom_dick_and_harry, _) }

  Seq(
    "t,d&h",
  ).foreach { show(tdh.tdh, _) }

  Seq(
    "t,d&h",
  ).foreach { show(tdh_left.tdh_left, _) }

  Seq(
    "1 + 0",
    "1 + 1 + 1 + 0",
    "0 + 0",
    "1 + 2",
  ).foreach { show(sum.sum, _) }

  Seq(
    "ab",
    "abab",
    "aabb",
    "aab",
  ).foreach { show(ab.ab, _) }

  Seq(
    "abc",
    "aabbcc",
  ).foreach { show(abc.abc, _) }
}
