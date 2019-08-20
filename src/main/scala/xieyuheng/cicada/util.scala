package xieyuheng.cicada

import scala.annotation.tailrec

object util {
  def newId(): Id = {
    java.util.UUID.randomUUID().toString
  }

  @tailrec
  def walk(x: Value, bind: Bind): Value = {
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
      case sumType: SumTypeValue => {
        val id = sumType.id
        bind.get(id) match {
          case Some(y) => walk(y, bind)
          case None => x
        }
      }
      case pi: PiValue => {
        val id = pi.id
        bind.get(id) match {
          case Some(y) => walk(y, bind)
          case None => x
        }
      }
      case _ => x
    }
  }

  def deepWalk(x: Value, bind: Bind): Value = {
    walk(x, bind) match {
      case t: TypeOfType => t
      case t: ValueOfType => t
      case memberType: MemberTypeValue =>
        // TODO prune the bind
        memberType.copy(map = deepWalkForMap(memberType.map, bind))
      case sumType: SumTypeValue =>
        // TODO prune the bind
        sumType.copy(map = deepWalkForMap(sumType.map, bind))
      case pi: PiValue =>
        // TODO prune the bind
        pi.copy(
          args = deepWalkForMap(pi.args, bind),
          ret = deepWalk(pi.ret, bind))
      case fn: FnValue =>
        fn.copy(
          args = deepWalkForMap(fn.args, bind),
          ret = deepWalk(fn.ret, bind))
      case neu: NeutralValue =>
        neu
    }
  }

  def deepWalkForMap(map: MultiMap[String, Value], bind: Bind): MultiMap[String, Value] = {
    map.mapValues(deepWalk(_, bind))
  }
}
