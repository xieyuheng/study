package cicada

case class MultiMap[K, V](
  val entries: List[(K, V)] = List(),
) {
  var valuesMap: Map[K, List[V]] = Map()

  entries.foreach { case (k, v) =>
    valuesMap = valuesMap.get(k) match {
      case Some(values) =>
        valuesMap + (k -> (v :: values))
      case None =>
        valuesMap + (k -> List(v))
    }
  }

  def entriesInScope: List[(K, V)] = {
    entries.toMap.toList
  }

  def update(kv: (K, V)): MultiMap[K, V] =
    MultiMap(entries ++ List(kv))

  def merge(map: MultiMap[K, V]): MultiMap[K, V] =
    MultiMap(entries ++ map.entries)

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

  def keys = valuesMap.keys

  def mapValues[A](f: V => A): MultiMap[K, A] =
    MultiMap(entries.map { case (k, v) => (k, f(v)) })

  def contains(k: K): Boolean = valuesMap.contains(k)

  def isEmpty: Boolean = entries.isEmpty
}

object MultiMap {
  def fromList[K, V](entries: List[(K, V)]): MultiMap[K, V] =
    new MultiMap(entries)

  def apply[K, V](entries: List[(K, V)]): MultiMap[K, V] =
    new MultiMap(entries)

  def apply[K, V](entries: (K, V)*): MultiMap[K, V] =
    new MultiMap(entries.toList)
}
