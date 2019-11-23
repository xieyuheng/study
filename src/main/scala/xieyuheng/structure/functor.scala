package xieyuheng.structure

abstract class functor_t {
  val dom: category_t
  val cod: category_t

  def map(x: dom.object_t): cod.object_t

  def fmap(f: dom.morphism_t): cod.morphism_t
  def fmap_compatible(f: dom.morphism_t) = {
    val a = dom.dom(f)
    val b = dom.cod(f)
    eqv(cod.dom(fmap(f)), map(a))
    eqv(cod.cod(fmap(f)), map(b))
  }

  def fmap_respect_compose(f: dom.morphism_t, g: dom.morphism_t) = {
    eqv[cod.morphism_t](
      fmap(dom.compose(f, g)),
      cod.compose(fmap(f), fmap(g)))
  }

  def fmap_respect_id(a: dom.object_t) = {
    eqv[cod.morphism_t](
      fmap(dom.id(a)),
      cod.id(map(a)))
  }
}
