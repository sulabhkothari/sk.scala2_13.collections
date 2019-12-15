import scala.collection.{IterableOps, MapOps, SortedSetOps, View}

object CollectionsArchitecture extends App {
  var x: IterableOps[Int, List[_], List[Int]] = _
  var y: MapOps[String, Int, Map[_, _], Map[String, Int]] = _
  val m = Map[Int, String]()
  /**
    * trait Map[K, V] extends Iterable[(K, V)] with MapOps[K, V, Map, Map[K, V]]
    *
    * trait MapOps[K, +V, +CC[_, _], +C] extends IterableOps[(K, V), Iterable, C] {
    * def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] = …
    * }
    *
    * // from MapOps
    * def map[K2, V2](f: ((K, V)) => (K2, V2)): Map[K2, V2]
    *
    * // from IterableOps
    * def map[B](f: ((K, V)) => B): Iterable[B]
    *
    * At use-site, when you call the map operation, the compiler selects one of the two overloads. If the function
    * passed as argument to map returns a pair, both functions are applicable. In this case, the version from MapOps is
    * used because it is more specific by the rules of overloading resolution, so the resulting collection is a Map. If
    * the argument function does not return a pair, only the version defined in IterableOps is applicable. In this case,
    * the resulting collection is an Iterable. This is how we follow the “same-result-type” principle: wherever possible
    * a transformation method on a collection yields a collection of the same type.
    *
    *
    * trait SortedSet[A] extends SortedSetOps[A, SortedSet, SortedSet[A]]
    *
    * trait SortedSetOps[A, +CC[_], +C] extends IterableOps[A, Set, C] {
    *
    * def map[B](f: A => B)(implicit ord: Ordering[B]): CC[B] = …
    *
    * }
    *
    * trait SortedSet[A] extends SortedSetOps[A, SortedSet, SortedSet[A]]
    **/
}
