package xieyuheng.cicada

object util {
  def list_replace[A](list: List[A], index: Int, x: A): List[A] = {
    list.patch(index, List(x), 1)
  }

  def unwrap[A](either: Either[Err, A]): A = {
    either match {
      case Right(a) => a
      case Left(err) =>
        println(s"[unwrap fail]")
        println(s"error: ${err.msg}")
        throw new Exception()
    }
  }
}
