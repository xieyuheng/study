package xieyuheng.minitt

import xieyuheng.partech.Parser

object minitt {

  def version: String = "0.0.1"

  def opt(args: Array[String], name: String, arity: Int): Option[Array[String]] = {
    val i = args.indexOf(name)
    if (i == -1) {
      None
    } else {
      Some(args.slice(i + 1, i + 1 + arity))
    }
  }

  def print_help(): Unit = {
    val usage = s"""
        |minitt ${version}
        |
        |usage:
        |  -e, --eval <file_name> [default]
        |  -v, --version
        |  -h, --help
        """.stripMargin
    println(usage)
  }

  def main(args: Array[String]): Unit = {

    opt(args, "-h", 0).foreach {
      case _ =>
        print_help()
        System.exit(0)
    }

    opt(args, "--help", 0).foreach {
      case _ =>
        print_help()
        System.exit(0)
    }

    opt(args, "-v", 0).foreach {
      case _ =>
        println(version)
        System.exit(0)
    }

    opt(args, "--version", 0).foreach {
      case _ =>
        println(version)
        System.exit(0)
    }

    opt(args, "--eval", 1).foreach {
      case Array(file_name) =>
        run_file(file_name)
        System.exit(0)
    }

    if (args.length == 1) {
      val file_name = args(0)
      run_file(file_name)
      System.exit(0)
    }

    print_help()

    System.exit(0)
  }

  def run_file(file_name: String): Unit = {
    val path = os.Path(file_name, base = os.pwd)

    if (!os.isFile(path)) {
      println(s"not a file: ${path}")
      System.exit(1)
    }

    val code = os.read(path)

    Parser(grammar.lexer, grammar.module).parse(code) match {
      case Right(tree) =>
        val module = grammar.module_matcher(tree)
        module.check()
        module.run()
      case Left(error) =>
        println(s"[parse_error] ${error.msg}")
        println(s"- file: ${file_name}")
        System.exit(1)
    }
  }

}
