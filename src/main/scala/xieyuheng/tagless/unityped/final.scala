package xieyuheng.tagless.unityped.finial

trait BasicSemantics {
  type Exp
  def lit(int: Int): Exp
  def neg(x: Exp): Exp
  def add(x: Exp, y: Exp): Exp
}

object BasicExample {
  def tf1[S <: BasicSemantics](implicit semantics: S): S#Exp = {
    import semantics._
    add(lit(8), neg(add(lit(1), lit(2))))
  }
}

class BasicEval extends BasicSemantics {
  type Exp = Int
  def lit(int: Int): Int = int
  def neg(x: Int): Int = -x
  def add(x: Int, y: Int): Int = x + y
}

class BasicView extends BasicSemantics {
  type Exp = String
  def lit(int: Int): String = s"${int}"
  def neg(x: String): String = s"(-${(x)})"
  def add(x: String, y: String): String = s"(${x}+${y})"
}

trait MulSemantics {
  type Exp
  def mul(x: Exp, y: Exp): Exp
}

object MulExample {
  def tfm1[S <: MulSemantics with BasicSemantics](implicit semantics: S): S#Exp = {
    import semantics._
    add(lit(7), neg(mul(lit(1), lit(2))))
  }

  def tfm2[S <: MulSemantics with BasicSemantics](implicit semantics: S): S#Exp = {
    import semantics._
    mul(lit(7), BasicExample.tf1(semantics))
  }
}

class MulEval extends BasicEval with MulSemantics {
  def mul(x: Int, y: Int): Int = x * y
}

class MulView extends BasicView with MulSemantics {
  def mul(x: String, y: String): String = s"(${x}*${y})"
}

object EvalApp extends App {
  // println(BasicExample.tf1(new BasicEval))
  // println(MulExample.tfm1(new MulEval))
  // println(MulExample.tfm2(new MulEval))
  implicit val basicEval = new BasicEval
  implicit val mulEval = new MulEval
  println(BasicExample.tf1)
  println(MulExample.tfm1)
  println(MulExample.tfm2)
}

object ViewApp extends App {
  // println(BasicExample.tf1(new BasicView))
  // println(MulExample.tfm1(new MulView))
  // println(MulExample.tfm2(new MulView))
  implicit val basicView = new BasicView
  implicit val mulView = new MulView
  println(BasicExample.tf1)
  println(MulExample.tfm1)
  println(MulExample.tfm2)
}

sealed trait Tree
final case class Leaf(name: String) extends Tree
final case class Node(name: String, child: List[Tree]) extends Tree

class BasicSer extends BasicSemantics {
  type Exp = Tree
  def lit(int: Int): Tree = Node("lit", List(Leaf(int.toString)))
  def neg(x: Tree): Tree = Node("neg", List(x))
  def add(x: Tree, y: Tree): Tree = Node("add", List(x, y))
}

class MulSer extends BasicSer with MulSemantics {
  def mul(x: Tree, y: Tree): Tree = Node("mul", List(x, y))
}

object SerApp extends App {
  implicit val basicSer = new BasicSer
  implicit val mulSer = new MulSer
  println(BasicExample.tf1)
  println(MulExample.tfm1)
  println(MulExample.tfm2)
}

object de {
  def fromTree[S <: BasicSemantics](tree: Tree)
    (implicit semantics: S): Either[String, semantics.Exp] = {
    tree match {
      case Node("lit", List(Leaf(intString))) =>
        Right(semantics.lit(intString.toInt))
      case Node("neg", List(xSer)) =>
        for {
          xDe <- fromTree(xSer)
        } yield semantics.neg(xDe)
      case Node("add", List(xSer, ySer)) =>
        for {
          xDe <- fromTree(xSer)
          yDe <- fromTree(ySer)
        } yield semantics.add(xDe, yDe)
      case tree =>
        Left(tree.toString)
    }
  }
}

object deApp extends App {
  println(BasicExample.tf1(new BasicSer))
  println(de.fromTree(BasicExample.tf1(new BasicSer))(new BasicEval))
  println(de.fromTree(BasicExample.tf1(new BasicSer))(new BasicView))
}

object de1 {
  /** This is not De! What is this? */

  trait BasicDe[S <: BasicSemantics] extends BasicSemantics {
    val semantics: S

    type Exp = Tree => Either[String, semantics.Exp]

    def lit(int: Int): Exp = {
      case Node("lit", List(Leaf(intString))) if intString == int.toString =>
        Right(semantics.lit(int))
      case tree =>
        Left(tree.toString)
    }

    def neg(x: Exp): Exp = {
      case Node("neg", List(xSer)) =>
        for {
          xDe <- x(xSer)
        } yield semantics.neg(xDe)
      case tree =>
        Left(tree.toString)
    }

    def add(x: Exp, y: Exp): Exp = {
      case Node("add", List(xSer, ySer)) =>
        for {
          xDe <- x(xSer)
          yDe <- y(ySer)
        } yield semantics.add(xDe, yDe)
      case tree =>
        Left(tree.toString)
    }
  }

  trait MulDe[S <: BasicSemantics with MulSemantics] extends BasicDe[S] with MulSemantics {
    val semantics: S

    def mul(x: Exp, y: Exp): Exp = {
      case Node("mul", List(xSer, ySer)) =>
        for {
          xDe <- x(xSer)
          yDe <- y(ySer)
        } yield semantics.mul(xDe, yDe)
      case tree =>
        Left(tree.toString)
    }
  }

  object DeApp extends App {
    println(BasicExample.tf1(new BasicSer))
    println(MulExample.tfm1(new MulSer))
    println(MulExample.tfm2(new MulSer))

    implicit val toBasicEval = new BasicDe[BasicEval] {
      val semantics = new BasicEval
    }

    println(BasicExample.tf1(toBasicEval)(BasicExample.tf1(new BasicSer)))

    implicit val toBasicView = new BasicDe[BasicView] {
      val semantics = new BasicView
    }

    println(BasicExample.tf1(toBasicView)(BasicExample.tf1(new BasicSer)))

    implicit val toMulEval = new MulDe[MulEval] {
      val semantics = new MulEval
    }

    println(MulExample.tfm1(toMulEval)(MulExample.tfm1(new MulSer)))
    println(MulExample.tfm2(toMulEval)(MulExample.tfm2(new MulSer)))

    implicit val toMulView = new MulDe[MulView] {
      val semantics = new MulView
    }

    println(MulExample.tfm1(toMulView)(MulExample.tfm1(new MulSer)))
    println(MulExample.tfm2(toMulView)(MulExample.tfm2(new MulSer)))
  }

}
