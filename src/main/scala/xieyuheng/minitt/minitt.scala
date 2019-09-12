package xieyuheng.minitt

import xieyuheng.partech.Parser

object minitt {

  def version: String = "0.0.1"

  def get_args(args: Array[String], name: String, arity: Int): Option[Array[String]] = {
    val i = args.indexOf(name)
    if (i == -1) {
      None
    } else {
      Some(args.slice(i + 1, i + 1 + arity))
    }
  }

  def print_help(): Unit = {
    val usage = s"""
        |usage:
        |  --eval <file_name>
        """.stripMargin
    println(usage)
  }

  def main(args: Array[String]): Unit = {

    get_args(args, "--help", 0).foreach {
      case _ =>
        print_help()
        System.exit(0)
    }

    get_args(args, "--version", 0).foreach {
      case _ =>
        println(version)
        System.exit(0)
    }

    get_args(args, "--eval", 1).foreach {
      case Array(file_name) =>
        run_file(file_name)
        System.exit(0)
    }

    print_help()

    System.exit(0)
  }

  def run_file(file_name: String): Unit = {
    val code = os.read(os.Path(file_name, base = os.pwd))

    var module = Parser(grammar.lexer, grammar.module).parse(code) match {
      case Right(tree) => grammar.module_matcher(tree)
      case Left(error) =>
        println(s"[minitt] parse error")
        println(s"- file_name: ${file_name}")
        println(s"- code: ${code}")
        println(s"- error: ${error}")
        throw new Exception()
    }
    module.run()
  }

}
