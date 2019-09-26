package xieyuheng.cicada

object util {

  def list_replace[A](list: List[A], index: Int, x: A): List[A] = {
    list.patch(index, List(x), 1)
  }

  def result_unwrap[A](result: Either[Err, A]): A = {
    result match {
      case Right(a) => a
      case Left(err) =>
        println(s"[result_unwrap fail]")
        println(s"error: ${err.msg}")
        throw new Exception()
    }
  }

}
