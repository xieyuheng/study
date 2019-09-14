package xieyuheng.cicada.with_logic_variable

import scala.collection.immutable.ListMap

import xieyuheng.cicada.with_logic_variable.pretty._

object unify {
  def apply(src: Val, tar: Val, bind: Bind, env: Env): Either[Err, Bind] = {
    (walk(src, bind), walk(tar, bind)) match {
      case (src, tar) if {
        src == tar
      } => {
        Right(bind)
      }

      case (t1: TypeOfType, t2: TypeOfType) => {
        Right(bind.extend(t1.id -> t2))
      }

      case (sumType: SumTypeVal, t: TypeOfType) => {
        Right(bind.extend(t.id -> sumType))
      }

      case (t: TypeOfType, sumType: SumTypeVal) => {
        Right(bind.extend(t.id -> sumType))
      }

      case (memberType: MemberTypeVal, t: TypeOfType) => {
        for {
          sumType <- eval(Var(memberType.superName), env)
        } yield bind.extend(t.id -> sumType)
      }

      case (t: TypeOfType, memberType: MemberTypeVal) => {
        for {
          sumType <- eval(Var(memberType.superName), env)
        } yield bind.extend(t.id -> sumType)
      }

      case (pi: PiVal, t: TypeOfType) => {
        Right(bind.extend(t.id -> pi))
      }

      case (t: TypeOfType, pi: PiVal) => {
        Right(bind.extend(t.id -> pi))
      }

      case (value, ValOfType(id, t)) => {
        for {
          bind <- unify(value, t, bind, env)
        } yield bind.extend(id -> value)
      }

      case (ValOfType(id, t), value) => {
        for {
          bind <- unify(value, t, bind, env)
        } yield bind.extend(id -> value)
      }

      case (fn: FnVal, pi: PiVal) => {
        for {
          /** contravariant at args */
          bind <- unify.onMap(pi.args, fn.args, bind, env)
          bind <- unify(fn.ret, pi.ret, bind, env)
        } yield bind
      }

      case (pi: PiVal, fn: FnVal) => {
        for {
          /** contravariant at args */
          bind <- unify.onMap(pi.args, fn.args, bind, env)
          bind <- unify(fn.ret, pi.ret, bind, env)
        } yield bind
      }

      case (memberType: MemberTypeVal, sumType: SumTypeVal) if {
        sumType.memberNames.contains(memberType.name)
      } => {
        for {
          bind <- unify.onBind(memberType.bind, bind, env)
          bind <- unify.onBind(sumType.bind, bind, env)
          bind <- unify.onMap(memberType.map, sumType.map, bind, env)
        } yield bind
      }

      case (sumType: SumTypeVal, memberType: MemberTypeVal) if {
        sumType.memberNames.contains(memberType.name)
      } => {
        for {
          bind <- unify.onBind(memberType.bind, bind, env)
          bind <- unify.onBind(sumType.bind, bind, env)
          bind <- unify.onMap(memberType.map, sumType.map, bind, env)
        } yield bind
      }

      case (src: SumTypeVal, tar: SumTypeVal) if {
        src.name == tar.name
      } => {
        for {
          bind <- unify.onBind(src.bind, bind, env)
          bind <- unify.onBind(tar.bind, bind, env)
          bind <- unify.onMap(src.map, tar.map, bind, env)
        } yield bind
      }

      case (src: MemberTypeVal, tar: MemberTypeVal) if {
        src.name == tar.name
      } => {
        for {
          bind <- unify.onBind(src.bind, bind, env)
          bind <- unify.onBind(tar.bind, bind, env)
          bind <- unify.onMap(src.map, tar.map, bind, env)
        } yield bind
      }

      case (src: PiVal, tar: PiVal) => {
        for {
          bind <- unify.onMap(tar.args, src.args, bind, env)
          bind <- unify(src.ret, tar.ret, bind, env)
        } yield bind
      }

      case _ => {
        Left(Err(
          "fail to unify\n" ++
            s"src: ${prettyVal(walk(src, bind))}\n" ++
            s"tar: ${prettyVal(walk(tar, bind))}\n" ++
            s"bind: ${prettyBind(bind)}\n"))
      }
    }
  }

  def onMap(
    srcMap: ListMap[String, Val],
    tarMap: ListMap[String, Val],
    bind: Bind,
    env: Env,
  ): Either[Err, Bind] = {
    val initResult: Either[Err, Bind] =
      Right(bind)

    def updateBind(
      bind: Bind,
      name: String,
      tarVal: Val,
    ): Either[Err, Bind] = {
      srcMap.get(name) match {
        case Some(srcVal) =>
          unify(srcVal, tarVal, bind, env)
        case None =>
          Right(bind)
      }
    }

    tarMap.foldLeft(initResult) { case (result, (name, tarVal)) =>
      result.flatMap { bind => updateBind(bind, name, tarVal) }
    }
  }

  def onBind(
    bind1: Bind,
    bind2: Bind,
    env: Env,
  ): Either[Err, Bind] = {
    val missingInBind1 = bind2.filterNot { case (id, _value) => bind1.keys.toSet.contains(id) }

    val initBind = bind1.extendByBind(missingInBind1)

    assert(initBind.size >= bind1.size)

    val initResult: Either[Err, Bind] =
      Right(initBind)

    def updateBind(bind: Bind, id: Id, v: Val): Either[Err, Bind] = {
      bind.get(id) match {
        case Some(v1) =>
          unify(v1, v, bind, env)
        case None =>
          Left(Err(s"onBind internal error, bind1: ${bind1}, bind2: ${bind2}"))
      }
    }

    bind2
      .filter { case (id, _value) => bind1.keys.toSet.contains(id) }
      .map.foldLeft(initResult) { case (result, (id, v)) =>
        result.flatMap { bind => updateBind(bind, id, v) }
      }
  }
}
