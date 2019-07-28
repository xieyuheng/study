package xieyuheng.cicada

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object fulfill {
  @tailrec
  def walk(x: Value, unifEnv: Map[Value, Value]): Value = {
    unifEnv.get(x) match {
      case Some(y) => walk(y, unifEnv)
      case None => x
    }
  }

  def fulfill(
    src: Value,
    tar: Value,
    unifEnv: Map[Value, Value],
  ): Either[ErrorMsg, Map[Value, Value]] = {
    (walk(src, unifEnv), walk(tar, unifEnv)) match {
      case (TypeValue(id), TypeValue(id2)) if id == id2 =>
        Right(unifEnv)
      case (value, t: TypeValue) =>
        Right(unifEnv + (t -> value))
      case (fn: FnValue, pi: PiValue) =>
        for {
          unifEnv1 <- fulfillMap(pi.args, fn.args, unifEnv)
          unifEnv2 <- fulfill(fn.ret, pi.ret, unifEnv1)
        } yield unifEnv2 + (fn -> pi)
      case (record: RecordValue, union: UnionValue) if union.subNames contains record.name =>
        for {
          unifEnv1 <- fulfillMap(record.map, union.map, unifEnv)
        } yield unifEnv1 + (record -> union)
      case (src: UnionValue, tar: UnionValue) if src.name == tar.name =>
        fulfillMap(src.map, tar.map, unifEnv)
      case (src: RecordValue, tar: RecordValue) if src.name == tar.name =>
        fulfillMap(src.map, tar.map, unifEnv)
      case (src: PiValue, tar: PiValue) =>
        for {
          unifEnv1 <- fulfillMap(tar.args, src.args, unifEnv)
          unifEnv2 <- fulfill(src.ret, tar.ret, unifEnv1)
        } yield unifEnv2
      case (src, tar) if src == tar =>
        Right(unifEnv)
      case _ =>
        Left(ErrorMsg())
    }
  }

  def fulfillMap(
    srcMap: ListMap[String, Value],
    tarMap: ListMap[String, Value],
    unifEnv: Map[Value, Value],
  ): Either[ErrorMsg, Map[Value, Value]] = {
    val initResult: Either[ErrorMsg, Map[Value, Value]] = Right(Map())
    tarMap.foldLeft(initResult) { case (result, (name, tarValue)) =>
      for {
        unifEnv1 <- result
        unifEnv2 <- srcMap.get(name) match {
          case Some(srcValue) => fulfill(srcValue, tarValue, unifEnv1)
          case None => Left(ErrorMsg())
        }
      } yield unifEnv2
    }
  }
}
