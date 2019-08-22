package cicada

import scala.annotation.tailrec

import scala.collection.immutable.ListMap

object walk {
  @tailrec
  def apply(x: Value, bind: Bind): Value = {
    x match {
      case t: TypeOfType => {
        val id = t.id
        bind.get(id) match {
          case Some(y) => walk(y, bind)
          case None => x
        }
      }
      case t: ValueOfType => {
        val id = t.id
        bind.get(id) match {
          case Some(y) => walk(y, bind)
          case None => x
        }
      }
      case _ => x
    }
  }

  // TODO prune the bind
  def deep(x: Value, bind: Bind): Value = {
    walk(x, bind) match {
      case t: TypeOfType =>
        t
      case t: ValueOfType =>
        t.copy(t = deep(t.t, bind))
      case memberType: MemberTypeValue =>
        memberType.copy(map = deepOnMap(memberType.map, bind))
      case sumType: SumTypeValue =>
        sumType.copy(map = deepOnMap(sumType.map, bind))
      case pi: PiValue =>
        pi.copy(
          args = deepOnMap(pi.args, bind),
          ret = deep(pi.ret, bind))
      case fn: FnValue =>
        fn.copy(
          args = deepOnMap(fn.args, bind),
          ret = deep(fn.ret, bind))
      case neu: NeutralValue =>
        neu
    }
  }

  def deepOnMap(map: ListMap[String, Value], bind: Bind): ListMap[String, Value] = {
    ListMap(map.mapValues(deep(_, bind)).toList: _*)
  }
}
