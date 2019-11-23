package xieyuheng.structure

abstract class category_t {
  type object_t
  type morphism_t

  def dom(morphism: morphism_t): object_t
  def cod(morphism: morphism_t): object_t

  def id(x: object_t): morphism_t
  def id_compatible(x: object_t) = {
    eqv(x, dom(id(x)))
    eqv(x, cod(id(x)))
  }

  def compose(f: morphism_t, g: morphism_t): morphism_t
  def compose_compatible(f: morphism_t, g: morphism_t) = {
    eqv(cod(f), dom(g))
    eqv(dom(compose(f, g)), dom(f))
    eqv(cod(compose(f, g)), cod(g))
  }

  def id_left(f: morphism_t) =
    eqv(compose(id(dom(f)), f), f)

  def id_right(f: morphism_t) =
    eqv(compose(f, id(cod(f))), f)

  def compose_associative(f: morphism_t, g: morphism_t, h: morphism_t) =
    eqv[morphism_t](
      compose(f, compose(g, h)),
      compose(compose(f, g), h))
}
