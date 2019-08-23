package xieyuheng.cicada

import scala.collection.immutable.ListMap

object unifyTest extends App {
  val `unify should not make bind weaker` = {
    val id = Id()
    val bind = Bind(Map(
      id -> SumTypeValue(
        "nat_t",
        ListMap(),
        List("zero_t", "succ_t"),
        Bind())))

    val srcValue = MemberTypeValue("zero_t", ListMap(), "list_t", Bind())

    val tarValue = TypeOfType(id)

    unify(srcValue, tarValue, bind, Env()) match {
      case Right(newBind) =>
        assert(bind.toSet.subsetOf(newBind.toSet))
      case Left(errorMsg) =>
        throw new Exception(errorMsg.toString)
    }
  }
}
