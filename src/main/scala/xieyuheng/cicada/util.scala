package xieyuheng.cicada

object util {
  def list_replace[A](list: List[A], index: Int, x: A): List[A] = {
    list.patch(index, List(x), 1)
  }
}
