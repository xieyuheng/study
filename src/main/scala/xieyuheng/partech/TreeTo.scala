package xieyuheng.partech

trait TreeTo[A] {
  def treeTo(tree: Tree): A

  def apply(tree: Tree): A = treeTo(tree)
}

object TreeTo {
  def apply[A](f: Tree => A): TreeTo[A] = {
    new TreeTo[A] {
      def treeTo(tree: Tree): A = f(tree)
    }
  }

  def fromMatcher[A](
    name: String,
    map: Map[String, List[Tree] => A],
  ): TreeTo[A] = TreeTo[A] {
    case Node(rule, choiceName, children) =>
      if (rule.name == name) {
        map.get(choiceName) match {
          case Some(f) => f(children)
          case None => throw new Exception()
        }
      } else {
        throw new Exception()
      }
    case _ => throw new Exception()
  }

}
