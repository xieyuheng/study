package xieyuheng.cicada

case class Err(msg: String) {

  def append_cause(cause: Err): Err = {
    Err(
      msg ++
        "------------\n" ++
        cause.msg
    )
  }

}
