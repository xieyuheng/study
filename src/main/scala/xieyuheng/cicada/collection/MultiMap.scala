package xieyuheng.cicada

case class MultiMap[K, V](
  val entries: List[(K, V)] = List(),
) {
  var valuesMap: Map[K, List[V]] = Map()

  entries.reverse.foreach { case (k, v) =>
    valuesMap = valuesMap.updatedWith(k) {
      case Some(values) => Some(v :: values)
      case None => Some(List(v))
    }
  }

  def update(kv: (K, V)): MultiMap[K, V] =
    MultiMap(kv :: entries)

  def merge(mmap: MultiMap[K, V]): MultiMap[K, V] =
    MultiMap(mmap.entries ++ entries)

  def getList(k: K): List[V] = {
    valuesMap.get(k) match {
      case Some(values) => values
      case None => List()
    }
  }

  def get(k: K): Option[V] = {
    val values = getList(k)
    if (values.isEmpty) {
      None
    } else {
      Some(values.head)
    }
  }

  def mapValues[A](f: V => A): MultiMap[K, A] =
    MultiMap(entries.map { case (k, v) => (k, f(v)) })
}

object MultiMap {
  def fromList[K, V](entries: List[(K, V)]): MultiMap[K, V] =
    new MultiMap(entries)
}
