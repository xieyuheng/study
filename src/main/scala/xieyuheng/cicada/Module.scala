package xieyuheng.cicada

import xieyuheng.partech.Parser

// import check._
import pretty._
import readback._

case class Module(file_path: String) {

  var top_list: List[Top] = List()

  def env: Env = {
    var env: Env = Env()
    top_list.foreach {
      case TopDecl(decl) =>
        env = env.ext_decl(decl)
      case TopImportAll(path) =>
        env = import_file(path, env)
      case _ => {}
    }
    env
  }

  def run(): Unit = {
    var env: Env = Env()
    top_list.foreach {
      case TopDecl(decl) =>
        env = env.ext_decl(decl)
      case TopImportAll(path) =>
        env = import_file(path, env)
      case TopEval(exp) =>
        eval_print(exp)
      case TopEq(e1, e2) =>
        assert_eq(e1, e2)
      case TopNotEq(e1, e2) =>
        assert_not_eq(e1, e2)
      case _ => {}
    }
  }

  def assert_not_eq(e1: Exp, e2: Exp): Unit = {
    val v1 = eval(e1, env)
    val v2 = eval(e2, env)
    val n1 = readback_val(init_seed(), v1)
    val n2 = readback_val(init_seed(), v2)
    if (v1 == v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be not equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_val(v1)}")
      // println(s"=== ${pretty_norm(n1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_val(v2)}")
      // println(s"=== ${pretty_norm(n2)}")
      throw new Exception()
    }
  }

  def assert_eq(e1: Exp, e2: Exp): Unit = {
    val v1 = eval(e1, env)
    val v2 = eval(e2, env)
    // val n1 = readback_val(v1, Set())
    // val n2 = readback_val(v2, Set())
    if (v1 != v2) {
      println(s"[assertion fail]")
      println(s"the following two expressions are asserted to be equal")
      println(s">>> ${pretty_exp(e1)}")
      println(s"=== ${pretty_val(v1)}")
      // println(s"=== ${pretty_exp(n1)}")
      println(s">>> ${pretty_exp(e2)}")
      println(s"=== ${pretty_val(v2)}")
      // println(s"=== ${pretty_exp(n2)}")
      throw new Exception()
    }
  }

  def eval_print(exp: Exp): Unit = {
    val value = eval(exp, env)
    val norm = readback_val(init_seed(), value)
    println(s">>> ${pretty_exp(exp)}")
    println(s"=== ${pretty_val(value)}")
    println(s"=== ${norm}")
    // println(s"=== ${pretty_norm(norm)}")
    println()
  }

  def load_code(code: String): Unit = {
    Parser(grammar.lexer, grammar.top_list).parse(code) match {
      case Right(tree) =>
        top_list = top_list ++ grammar.top_list_matcher(tree)
      case Left(error) =>
        println(s"[parse_error] ${error.msg}")
        throw new Exception()
    }
  }

  def module_path: os.Path = {
    os.Path(file_path, base = os.pwd)
  }

  def load_file(file_path: String): Unit = {
    val path = os.Path(file_path, base = os.pwd)

    if (!os.isFile(path)) {
      println(s"not a file: ${module_path}")
      System.exit(1)
    }

    val code = os.read(path)
    load_code(code)
  }

  def import_file(file_path: String, env: Env): Env = {
    val path = os.Path(file_path, base = module_path / os.up)
    val module = Module(path.toString)
    env.append(module.env)
  }

  def init(): Unit = {
    load_file(file_path)
  }

  init()
}
