package xieyuheng.cicada

import collection.immutable.ListMap

object util {

  def list_map_foreach_maybe_err[K, A]
    (list_map: ListMap[K, A])
    (f: (K, A) => Either[Err, Unit])
      : Either[Err, Unit] = {
    val init_result: Either[Err, Unit] = Right(())
    list_map.foldLeft(init_result) {
      case (result, (k, a)) =>
        result match {
          case Right(_ok) =>
            f(k, a) match {
              case Right(ok) => Right(ok)
              case Left(err) => Left(err)
            }
          case Left(err) => Left(err)
        }
    }
  }

  def list_map_map_maybe_err[K, A, B]
    (list_map: ListMap[K, A])
    (f: (K, A) => Either[Err, B])
      : Either[Err, ListMap[K, B]] = {
    val init_result: Either[Err, ListMap[K, B]] = Right(ListMap.empty)
    list_map.foldLeft(init_result) {
      case (result, (k, a)) =>
        result match {
          case Right(list_map) =>
            f(k, a) match {
              case Right(b) => Right(list_map + (k -> b))
              case Left(err) => Left(err)
            }
          case Left(err) => Left(err)
        }
    }
  }

  def list_map_map_entry_with_index_maybe_err[K, A, B]
    (list_map: ListMap[K, A])
    (f: (Int, K, A) => Either[Err, (K, B)])
      : Either[Err, ListMap[K, B]] = {
    val init_result: Either[Err, ListMap[K, B]] = Right(ListMap.empty)
    var i = 0
    list_map.foldLeft(init_result) {
      case (result, (k, a)) =>
        i = i + 1
        result match {
          case Right(list_map) =>
            f(i - 1, k, a) match {
              case Right((k, b)) => Right(list_map + (k -> b))
              case Left(err) => Left(err)
            }
          case Left(err) => Left(err)
        }
    }
  }

  def list_map_maybe_err[A, B]
    (list: List[A])
    (f: A => Either[Err, B])
      : Either[Err, List[B]] = {
    val init: Either[Err, List[B]] = Right(List.empty)
    list.foldLeft(init) {
      case (result, a) =>
        result match {
          case Right(list) =>
            f(a) match {
              case Right(b) => Right(list :+ b)
              case Left(err) => Left(err)
            }
          case Left(err) => Left(err)
        }
    }
  }

}
