package xieyuheng.partech

trait FromTree[A] {
  def fromTree(tree: Tree): A

  def apply(tree: Tree): A = fromTree(tree)
}
