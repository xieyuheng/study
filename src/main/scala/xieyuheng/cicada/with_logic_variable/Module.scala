package xieyuheng.cicada.with_logic_variable

import xieyuheng.util.pretty._
import xieyuheng.cicada.with_logic_variable.pretty._

case class Module() {
  var env: Env = Env()

  def define_value(
    name: String,
    value: Val,
  ): Unit = {
    env = env.extend(name -> DefineVal(name, value))
  }

  def define(
    name: String,
    exp: Exp,
  ): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        env = env.extend(name -> DefineVal(name, value))
      case Left(errorMsg) =>
        println(errorMsg)
    }
  }

  def define_member_type(
    name: String,
    map: MultiMap[String, Exp],
    superName: String,
  ): Unit = {
    env = env.extend(name -> DefineMemberType(name, map, superName))
  }

  def define_sum_type(
    name: String,
    map: MultiMap[String, Exp],
    memberNames: List[String],
  ): Unit = {
    env = env.extend(name -> DefineSumType(name, map, memberNames))
  }

  def define_type(
    name: String,
    fields: MultiMap[String, Exp],
    members: MultiMap[String, MultiMap[String, Exp]],
  ): Unit = {
    define_sum_type(name, fields, members.keys.toList)
    members.entries.foreach { case (memberName, map) =>
      define_member_type(memberName, fields.merge(map), name)
    }
  }

  def define_fn(
    name: String,
    args: MultiMap[String, Exp],
    ret: Exp,
    body: Exp,
  ): Unit = {
    env = env.extend(name -> DefineFn(name, args, ret, body))
  }

  def import_all(that: Module): Unit = {
    that.env.map.foreach { case (name, newDefine) =>
      env.get(name) match {
        case Some(oldDefine) =>
          if (newDefine != oldDefine) {
            println("[warn]")
            println(s"- redefining:\n${add_indent_to_block(prettyDefine(newDefine), 1)}")
            println(s"- old definition:\n${add_indent_to_block(prettyDefine(oldDefine), 1)}")
          }
        case None => {}
      }
      env = env.extend(name -> newDefine)
    }
  }

  def eval_print(exp: Exp): Unit = {
    eval(exp, env) match {
      case Right(value) =>
        println(s"==> ${pretty_val(value)}")
      case Left(errorMsg) =>
        println(s"??> ${errorMsg}")
    }
  }

  def eval_on_right[A](exp: Exp)(f: Val => A): A = {
    eval(exp, env) match {
      case Right(value) => f(value)
      case Left(errorMsg) =>
        throw new Exception(s"${errorMsg}")
    }
  }

}
