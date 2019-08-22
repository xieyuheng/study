package xieyuheng.cicada

import scala.collection.immutable.ListMap

import xieyuheng.cicada.pretty._

object unify {
  def apply(src: Value, tar: Value, bind: Bind, env: Env): Either[ErrorMsg, Bind] = {
    (walk(src, bind), walk(tar, bind)) match {
      case (src, tar) if {
        src == tar
      } => {
        Right(bind)
      }

      case (t1: TypeOfType, t2: TypeOfType) => {
        Right(bind.extend(t1.id -> t2))
      }

      case (sumType: SumTypeValue, t: TypeOfType) => {
        Right(bind.extend(t.id -> sumType))
      }

      case (t: TypeOfType, sumType: SumTypeValue) => {
        Right(bind.extend(t.id -> sumType))
      }

      case (memberType: MemberTypeValue, t: TypeOfType) => {
        for {
          sumType <- eval(Var(memberType.superName), env)
        } yield bind.extend(t.id -> sumType)
      }

      case (t: TypeOfType, memberType: MemberTypeValue) => {
        for {
          sumType <- eval(Var(memberType.superName), env)
        } yield bind.extend(t.id -> sumType)
      }

      case (pi: PiValue, t: TypeOfType) => {
        Right(bind.extend(t.id -> pi))
      }

      case (t: TypeOfType, pi: PiValue) => {
        Right(bind.extend(t.id -> pi))
      }

      case (value, ValueOfType(id, t)) => {
        for {
          bind <- unify(value, t, bind, env)
        } yield bind.extend(id -> value)
      }

      case (ValueOfType(id, t), value) => {
        for {
          bind <- unify(value, t, bind, env)
        } yield bind.extend(id -> value)
      }

      case (fn: FnValue, pi: PiValue) => {
        for {
          /** contravariant at args */
          bind <- unify.onMap(pi.args, fn.args, bind, env)
          bind <- unify(fn.ret, pi.ret, bind, env)
        } yield bind
      }

      case (pi: PiValue, fn: FnValue) => {
        for {
          /** contravariant at args */
          bind <- unify.onMap(pi.args, fn.args, bind, env)
          bind <- unify(fn.ret, pi.ret, bind, env)
        } yield bind
      }

      case (memberType: MemberTypeValue, sumType: SumTypeValue) if {
        sumType.memberNames.contains(memberType.name)
      } => {
        for {
          bind <- unify.onBind(memberType.bind, bind, env)
          bind <- unify.onBind(sumType.bind, bind, env)
          bind <- unify.onMap(memberType.map, sumType.map, bind, env)
        } yield bind
      }

      case (sumType: SumTypeValue, memberType: MemberTypeValue) if {
        sumType.memberNames.contains(memberType.name)
      } => {
        for {
          bind <- unify.onBind(memberType.bind, bind, env)
          bind <- unify.onBind(sumType.bind, bind, env)
          bind <- unify.onMap(memberType.map, sumType.map, bind, env)
        } yield bind
      }

      case (src: SumTypeValue, tar: SumTypeValue) if {
        src.name == tar.name
      } => {
        for {
          bind <- unify.onBind(src.bind, bind, env)
          bind <- unify.onBind(tar.bind, bind, env)
          bind <- unify.onMap(src.map, tar.map, bind, env)
        } yield bind
      }

      case (src: MemberTypeValue, tar: MemberTypeValue) if {
        src.name == tar.name
      } => {
        for {
          bind <- unify.onBind(src.bind, bind, env)
          bind <- unify.onBind(tar.bind, bind, env)
          bind <- unify.onMap(src.map, tar.map, bind, env)
        } yield bind
      }

      case (src: PiValue, tar: PiValue) => {
        for {
          bind <- unify.onMap(tar.args, src.args, bind, env)
          bind <- unify(src.ret, tar.ret, bind, env)
        } yield bind
      }

      case _ => {
        Left(ErrorMsg(
          "fail to unify\n" ++
            s"src: ${prettyValue(walk(src, bind))}\n" ++
            s"tar: ${prettyValue(walk(tar, bind))}\n" ++
            s"bind: ${prettyBind(bind)}\n"))
      }
    }
  }

  def onMap(
    srcMap: ListMap[String, Value],
    tarMap: ListMap[String, Value],
    bind: Bind,
    env: Env,
  ): Either[ErrorMsg, Bind] = {
    val initResult: Either[ErrorMsg, Bind] =
      Right(bind)

    def updateBind(
      bind: Bind,
      name: String,
      tarValue: Value,
    ): Either[ErrorMsg, Bind] = {
      srcMap.get(name) match {
        case Some(srcValue) =>
          unify(srcValue, tarValue, bind, env)
        case None =>
          Right(bind)
      }
    }

    tarMap.foldLeft(initResult) { case (result, (name, tarValue)) =>
      result.flatMap { bind => updateBind(bind, name, tarValue) }
    }
  }

  def onBind(
    bind1: Bind,
    bind2: Bind,
    env: Env,
  ): Either[ErrorMsg, Bind] = {
    val missingInBind1 = bind2.filterNot { case (id, _value) => bind1.keys.toSet.contains(id) }

    val initBind = bind1.extendByBind(missingInBind1)

    assert(initBind.size >= bind1.size)

    val initResult: Either[ErrorMsg, Bind] =
      Right(initBind)

    def updateBind(bind: Bind, id: Id, v: Value): Either[ErrorMsg, Bind] = {
      bind.get(id) match {
        case Some(v1) =>
          unify(v1, v, bind, env)
        case None =>
          Left(ErrorMsg(s"onBind internal error, bind1: ${bind1}, bind2: ${bind2}"))
      }
    }

    bind2
      .filter { case (id, _value) => bind1.keys.toSet.contains(id) }
      .map.foldLeft(initResult) { case (result, (id, v)) =>
        result.flatMap { bind => updateBind(bind, id, v) }
      }
  }
}
