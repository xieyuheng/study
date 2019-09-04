package xieyuheng.partech.lexerless

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
}
