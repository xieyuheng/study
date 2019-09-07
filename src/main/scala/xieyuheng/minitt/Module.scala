package xieyuheng.minitt

case class Module() {
  var declarations: List[Decl] = List()

  def let(name: String, t: Exp, e: Exp): Unit = {
    declarations = declarations :+ Let(name, t, e)
  }

  def letrec(name: String, t: Exp, e: Exp): Unit = {
    declarations = declarations :+ LetRec(name, t, e)
  }

}
