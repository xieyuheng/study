package xieyuheng.minitt

import xieyuheng.partech.Parser

object minitt {

  def main(args: Array[String]): Unit = {

    object maybe {
      var file_name: Option[String] = None
    }

    args.grouped(2).toList.foreach {
      case Array("--eval", file_name) =>
        maybe.file_name = Some(file_name)
      case Array(arg1, arg2) =>
        println(s"unknown argument pair: ${arg1} ${arg2}")
      case Array(arg) =>
        println(s"unknown argument: ${arg}")
    }

    maybe.file_name match {
      case Some(file_name) => run_file(file_name)
      case None =>
        val usage = s"""
        |usage:
        |  --eval <file_name>
        """.stripMargin

        println(usage)
    }

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
