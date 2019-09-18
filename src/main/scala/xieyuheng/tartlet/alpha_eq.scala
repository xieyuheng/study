package xieyuheng.tartlet

import java.util.UUID

object alpha_eq {

  def apply(
    x: Exp,
    y: Exp,
    x_map: Map[String, String],
    y_map: Map[String, String],
  ): Boolean =
    alpha_eq(x, y, x_map, y_map)

  def alpha_eq(
    x: Exp,
    y: Exp,
    x_map: Map[String, String],
    y_map: Map[String, String],
  ): Boolean = {
    (x, y) match {
      case (Var(name), Var(name2)) =>
        (x_map.get(name), x_map.get(name2)) match {
          case (Some(sym), Some(sym2)) =>
            sym == sym2
          case (None, None) =>
            name == name2
          case _ =>
            false
        }
      case (Atom(), Atom()) =>
        true
      case (Quote(sym), Quote(sym2)) =>
        sym == sym2
      case (Eqv(t, from, to), Eqv(t2, from2, to2)) =>
        alpha_eq(t, t2, x_map, y_map) &&
        alpha_eq(from, from2, x_map, y_map) &&
        alpha_eq(to, to2, x_map, y_map)
      case (Replace(target, motive, base), Replace(target2, motive2, base2)) =>
        alpha_eq(target, target2, x_map, y_map) &&
        alpha_eq(motive, motive2, x_map, y_map) &&
        alpha_eq(base, base2, x_map, y_map)
      case (Same(), Same()) =>
        true
      case (Succ(prev), Succ(prev2)) =>
        alpha_eq(prev, prev2, x_map, y_map)
      case (NatInd(target, motive, base, step), NatInd(target2, motive2, base2, step2)) =>
        alpha_eq(target, target2, x_map, y_map) &&
        alpha_eq(motive, motive2, x_map, y_map) &&
        alpha_eq(base, base2, x_map, y_map) &&
        alpha_eq(step, step2, x_map, y_map)
      case (Nat(), Nat()) =>
        true
      case (Zero(), Zero()) =>
        true
      case (Ap(rator, rand), Ap(rator2, rand2)) =>
        alpha_eq(rator, rator2, x_map, y_map) &&
        alpha_eq(rand, rand2, x_map, y_map)
      case (Fn(name, body), Fn(name2, body2)) =>
        val sym = UUID.randomUUID().toString
        alpha_eq(body, body2, x_map + (name -> sym), y_map + (name2 -> sym))
      case (Absurd(), Absurd()) =>
        true
      case (AbsurdInd(target, motive), AbsurdInd(target2, motive2)) =>
        alpha_eq(target, target2, x_map, y_map) &&
        alpha_eq(motive, motive2, x_map, y_map)
      case (Sigma(name, arg_t, dep_t), Sigma(name2, arg_t2, dep_t2)) =>
        val sym = UUID.randomUUID().toString
        alpha_eq(arg_t, arg_t2, x_map, y_map) &&
        alpha_eq(dep_t, dep_t2, x_map + (name -> sym), y_map + (name -> sym))
      case (Sole(), Sole()) =>
        true
      case (Trivial(), Trivial()) =>
        true
      case (Universe(), Universe()) =>
        true
      case (Pi(name, arg_t, dep_t), Pi(name2, arg_t2, dep_t2)) =>
        val sym = UUID.randomUUID().toString
        alpha_eq(arg_t, arg_t2, x_map, y_map) &&
        alpha_eq(dep_t, dep_t2, x_map + (name -> sym), y_map + (name -> sym))
      case (Car(pair), Car(pair2)) =>
        alpha_eq(pair, pair2, x_map, y_map)
      case (Cdr(pair), Cdr(pair2)) =>
        alpha_eq(pair, pair2, x_map, y_map)
      case (Cons(car, cdr), Cons(car2, cdr2)) =>
        alpha_eq(car, car2, x_map, y_map) &&
        alpha_eq(cdr, cdr2, x_map, y_map)
      case (The(t, value), The(t2, value2)) =>
        (t, t2) match {
          case (Absurd(), Absurd()) =>
            true
          case _ =>
            alpha_eq(t, t2, x_map, y_map) &&
            alpha_eq(value, value2, x_map, y_map)
        }
      case _ =>
        false
    }
  }
}
