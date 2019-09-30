package xieyuheng.cicada.telescope

object cicada {

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
        |cicada ${version}
        |
        |usage:
        |  -e, --eval <file_path> [default]
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
      case Array(file_path) =>
        run_file(file_path)
        System.exit(0)
    }

    if (args.length == 1) {
      val file_path = args(0)
      run_file(file_path)
      System.exit(0)
    }

    print_help()

    System.exit(0)
  }

  def run_file(file_path: String): Unit = {
    var module = Module(file_path)
    module.run()
  }

}
