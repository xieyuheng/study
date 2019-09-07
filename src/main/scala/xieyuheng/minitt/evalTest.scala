package xieyuheng.minitt

import xieyuheng.minitt.expDSL._

object evalTest extends Module with App {
  import_all(paper)

  println(run("id" $ "Bool" $ %("true")))

}
