package xieyuheng.cicada.with_logic_variable

import scala.annotation.tailrec

import scala.collection.immutable.ListMap

object walk {
  @tailrec
  def apply(x: Val, bind: Bind): Val = {
    x match {
      case t: TypeOfType => {
        val id = t.id
        bind.get(id) match {
          case Some(y) => walk(y, bind)
          case None => x
        }
      }
      case t: ValOfType => {
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
  def deep(x: Val, bind: Bind): Val = {
    walk(x, bind) match {
      case t: TypeOfType =>
        t
      case t: ValOfType =>
        t.copy(t = deep(t.t, bind))
      case memberType: MemberTypeVal =>
        memberType.copy(map = deepOnMap(memberType.map, bind))
      case sumType: SumTypeVal =>
        sumType.copy(map = deepOnMap(sumType.map, bind))
      case pi: PiVal =>
        pi.copy(
          args = deepOnMap(pi.args, bind),
          ret = deep(pi.ret, bind))
      case fn: FnVal =>
        fn.copy(
          args = deepOnMap(fn.args, bind),
          ret = deep(fn.ret, bind))
      case neu: NeuVal =>
        neu
      case v: Val =>
        v
    }
  }

  def deepOnMap(map: ListMap[String, Val], bind: Bind): ListMap[String, Val] = {
    ListMap(map.mapValues(deep(_, bind)).toList: _*)
  }

  def deepSelf(x: Val): Val = {
    x match {
      case memberType: MemberTypeVal =>
        memberType.copy(map = deepOnMap(memberType.map, memberType.bind))
      case sumType: SumTypeVal =>
        sumType.copy(map = deepOnMap(sumType.map, sumType.bind))
      case v: Val =>
        v
    }
  }
}
