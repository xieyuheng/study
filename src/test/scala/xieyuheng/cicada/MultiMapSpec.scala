import org.scalatest._
import xieyuheng.cicada._

class MultiMapSpec extends FlatSpec with Matchers {
  "MultiMap" should "update merge and get" in {
    val x = MultiMap.fromList(List(1 -> 1, 2 -> 2, 4 -> 4))

    val y = x.update(4 -> 5).update(4 -> 6)
    assert(y.get(4) == List(6, 5, 4))

    val z = x.merge(y)
    assert(z.get(4) == List(6, 5, 4, 4))
  }
}
