package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

object unifyTest extends App {
  val `unify should not make bind weaker` = {
    val id = Id()
    val bind = Bind(Map(
      id -> SumTypeVal(
        "nat_t",
        ListMap(),
        List("zero_t", "succ_t"),
        Bind())))

    val srcVal = MemberTypeVal("zero_t", ListMap(), "list_t", Bind())

    val tarVal = TypeOfType(id)

    unify(srcVal, tarVal, bind, Env()) match {
      case Right(newBind) =>
        assert(bind.toSet.subsetOf(newBind.toSet))
      case Left(errorMsg) =>
        throw new Exception(errorMsg.toString)
    }
  }
}
