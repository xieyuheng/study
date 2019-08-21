import org.scalatest._
import xieyuheng.cicada._

class unifySpec extends FlatSpec with Matchers {
  "unify" should "not make bind weaker" in {
    val id = Id()
    val bind = Map(
      id -> SumTypeValue(
        "Nat",
        MultiMap(List()),
        List("Zero", "Succ"),
        Map()))

    val srcValue = MemberTypeValue("Zero", MultiMap(List()), "List", Map())

    val tarValue = TypeOfType(id)

    unify(srcValue, tarValue, bind, Env()) match {
      case Right(newBind) =>
        assert(bind.toSet.subsetOf(newBind.toSet))
      case Left(errorMsg) =>
        throw new Exception(errorMsg.toString)
    }
  }
}
