import org.scalatest._
import xieyuheng.cicada._

class MultiMapSpec extends FlatSpec with Matchers {
  "MultiMap" can "update merge and get" in {
    val x = MultiMap.fromList(List(1 -> 1, 2 -> 2, 4 -> 4))

    val y = x.update(4 -> 5).update(4 -> 6)
    assert(y.getList(4) == List(6, 5, 4))

    val z = x.merge(y)
    assert(z.getList(4) == List(6, 5, 4, 4))
  }

  it can "contains" in {
    val x = MultiMap.fromList(List(1 -> 1, 2 -> 2, 4 -> 4))

    assert(x.contains(1))
    assert(x.contains(2))
    assert(x.contains(4))

    assert(!x.contains(5))
  }
}
