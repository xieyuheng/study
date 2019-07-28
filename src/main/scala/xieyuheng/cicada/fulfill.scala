package xieyuheng.cicada

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object fulfill {
  @tailrec
  def walk(
    x: Value,
    unifMap: Map[Value, Value],
  ): Value = {
    unifMap.get(x) match {
      case Some(y) => walk(y, unifMap)
      case None => x
    }
  }

  def fulfill(
    src: Value,
    tar: Value,
    unifMap: Map[Value, Value],
  ): Either[ErrorMsg, Map[Value, Value]] = {
    (walk(src, unifMap), walk(tar, unifMap)) match {
      case (src, tar)
          if src == tar =>
        Right(unifMap)
      case (value, t: TypeValue) =>
        Right(unifMap + (t -> value))
      case (fn: FnValue, pi: PiValue) =>
        for {
          /** contravariant at args */
          unifMap1 <- fulfillMap(pi.args, fn.args, unifMap)
          unifMap2 <- fulfill(fn.ret, pi.ret, unifMap1)
        } yield unifMap2 + (fn -> pi)
      case (record: RecordValue, union: UnionValue)
          if union.subNames contains record.name =>
        for {
          unifMap1 <- fulfillMap(record.map, union.map, unifMap)
        } yield unifMap1 + (record -> union)
      case (src: UnionValue, tar: UnionValue)
          if src.name == tar.name =>
        fulfillMap(src.map, tar.map, unifMap)
      case (src: RecordValue, tar: RecordValue)
          if src.name == tar.name =>
        fulfillMap(src.map, tar.map, unifMap)
      case (src: PiValue, tar: PiValue) =>
        for {
          unifMap1 <- fulfillMap(tar.args, src.args, unifMap)
          unifMap2 <- fulfill(src.ret, tar.ret, unifMap1)
        } yield unifMap2
      case _ =>
        Left(ErrorMsg(s"fail to fulfill src: ${src} into tar: ${tar}"))
    }
  }

  def fulfillMap(
    srcMap: ListMap[String, Value],
    tarMap: ListMap[String, Value],
    unifMap: Map[Value, Value],
  ): Either[ErrorMsg, Map[Value, Value]] = {
    val initResult: Either[ErrorMsg, Map[Value, Value]] = Right(Map())
    tarMap.foldLeft(initResult) { case (result, (name, tarValue)) =>
      for {
        unifMap1 <- result
        unifMap2 <- srcMap.get(name) match {
          case Some(srcValue) =>
            fulfill(srcValue, tarValue, unifMap1)
          case None =>
            Left(ErrorMsg(s"srcMap does not have name: ${name}, tarValue: ${tarValue}"))
        }
      } yield unifMap2
    }
  }

}
