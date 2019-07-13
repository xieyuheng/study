class Category (
  Object: Type
  Arrow (Object, Object): Type
  id (a: Object): Arrow (a, a)
  (|) (f: Arrow (a, b), g: Arrow (b, c)): Arrow(b, c)
  idLeft (f: Arrow (a, b)): id (a) | f == f
  idRight (f: Arrow (a, b)): f | id (b) == f
  assoc (
    f: Arrow (a, b),
    g: Arrow (b, c),
    h: Arrow (c, d),
  ): f | (g | h) == (f | g) | h
)

object Category {
  Inverse (f: Arrow (a, b), g: Arrow (a, b)): Type =
    (f | g == id (a),
     g | f == id (b))
}
