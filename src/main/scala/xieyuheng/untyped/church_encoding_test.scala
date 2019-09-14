package xieyuheng.untyped

object church_encoding_test extends App {
  val test_from_int = {
    val m = church_encoding.m
    m.run(church_encoding.from_int(0))
    m.run(church_encoding.from_int(1))
    m.run(church_encoding.from_int(2))
    m.run(church_encoding.from_int(3))
    m.run(Ap(Ap(Var("church_add"), church_encoding.from_int(2)), church_encoding.from_int(2)))
  }
}
