package xieyuheng.cicada

import scala.annotation.tailrec

object util {
  def newId(): Id = {
    java.util.UUID.randomUUID().toString
  }

  @tailrec
  def walk(x: Value, bind: Bind): Value = {
    x match {
      case t: LogicVar => {
        val id = t.id
        bind.get(id) match {
          case Some(y) => walk(y, bind)
          case None => x
        }
      }
      case union: UnionValue => {
        val id = union.id
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
      case t: LogicVar => walk(t, bind)
      case record: RecordValue =>
        // TODO prune the bind
        record.copy(map = deepWalkForMap(record.map, bind))
      case union: UnionValue =>
        // TODO prune the bind
        union.copy(map = deepWalkForMap(union.map, bind))
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
