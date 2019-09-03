package xieyuheng.partech

object TreeToTest extends App {
  import xieyuheng.partech.dsl._
  import example._

  def show[A](rule: Rule, text: String)
    (implicit treeTo: TreeTo[A])
      : Unit = {
    Parser(rule).parsing(text).nextTree match {
      case Some(tree) => println(Tree.to[A](tree))
      case None => println(s"[PARSING ERROR] rule: ${rule.name}, text: ${text}")
    }
  }

  bool_sexp.sentences.foreach { show[bool_sexp.BoolSexp](bool_sexp.main, _) }

//   Seq(
//     "tom, dick and harry",
//   ).foreach { show(tom_dick_and_harry.tom_dick_and_harry, _) }

//   Seq(
//     "t,d&h",
//   ).foreach { show(tdh.tdh, _) }

//   Seq(
//     "t,d&h",
//   ).foreach { show(tdh_left.tdh_left, _) }

  bin_sum.sentences.foreach { show[bin_sum.BinSum](bin_sum.main, _) }

//   Seq(
//     "ab",
//     "abab",
//     "aabb",
//     "aab",
//   ).foreach { show(ab.ab, _) }

//   Seq(
//     "abc",
//     "aabbcc",
//   ).foreach { show(abc.abc, _) }
}
