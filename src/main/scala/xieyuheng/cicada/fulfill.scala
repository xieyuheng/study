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

  def fulfill(src: Value, tar: Value, ctx: Ctx): Either[ErrorMessage, Ctx] = {
    (walk(src, ctx.unifEnv), walk(tar, ctx.unifEnv)) match {
      case (TypeValue(uuid), TypeValue(uuid2)) if uuid == uuid2 =>
        Right(ctx)
      case (value, t: TypeValue) =>
        Right(ctx.copy(unifEnv = ctx.unifEnv + (t -> value)))
      case (fn: FnValue, pi: PiValue) =>
        for {
          c1 <- fulfillMap(pi.args, fn.args, ctx)
          c2 <- fulfill(fn.ret, pi.ret, c1)
        } yield c2.copy(unifEnv = c2.unifEnv + (fn -> pi))
      case (record: RecordValue, union: UnionValue) if union.subNames contains record.name =>
        for {
          c1 <- fulfillMap(record.map, union.map, ctx)
        } yield c1.copy(unifEnv = c1.unifEnv + (record -> union))
      case (src: UnionValue, tar: UnionValue) if src.name == tar.name =>
        fulfillMap(src.map, tar.map, ctx)
      case (src: RecordValue, tar: RecordValue) if src.name == tar.name =>
        fulfillMap(src.map, tar.map, ctx)
      case (src: PiValue, tar: PiValue) =>
        for {
          c1 <- fulfillMap(tar.args, src.args, ctx)
          c2 <- fulfill(src.ret, tar.ret, c1)
        } yield c2
      case (src, tar) if src == tar =>
        Right(ctx)
      case _ =>
        Left(ErrorMessage())
    }
  }

  def fulfillMap(
    srcMap: ListMap[String, Value],
    tarMap: ListMap[String, Value],
    ctx: Ctx,
  ): Either[ErrorMessage, Ctx] = {
    val initResult: Either[ErrorMessage, Ctx] = Right(ctx)
    tarMap.foldLeft(initResult) { case (result, (name, tarValue)) =>
      for {
        c1 <- result
        c2 <- srcMap.get(name) match {
          case Some(srcValue) => fulfill(srcValue, tarValue, c1)
          case None => Left(ErrorMessage())
        }
      } yield c2
    }
  }
}
