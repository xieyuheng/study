import org.scalatest._
import xieyuheng.cicada._

class evalSpec extends FlatSpec with Matchers {
  "eval" should "eval Type to LogicVar" in {
    val env: Map[String, Def] = Map()
    for {
      t <- eval(Type(), env)
    } println(t)
  }
}
