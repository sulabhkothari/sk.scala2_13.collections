import scala.collection._
import scala.collection.mutable.{GrowableBuilder, Builder}

//Practical Algorithm to Retrieve Information Coded in Alphanumeric
//trie comes from retrieval (a trie is also called a radix tree or prefix tree)

class PrefixMap[A] private()
  extends mutable.Map[String, A]
    with mutable.MapOps[String, A, mutable.Map, PrefixMap[A]]
    with StrictOptimizedIterableOps[(String, A), mutable.Iterable, PrefixMap[A]] {

  private var suffixes: immutable.Map[Char, PrefixMap[A]] = immutable.Map.empty
  private var value: Option[A] = None

  def get(s: String): Option[A] = {
    if (s.isEmpty) value
    else suffixes.get(s(0)).flatMap(_.get(s.substring(1)))
  }

  def withPrefix(s: String): PrefixMap[A] = {
    //if(s.length > 0) println(suffixes(s(0)))
    if (s.isEmpty) this
    else {
      suffixes.get(s(0)) match {
        case None =>
          suffixes += s(0) -> empty
        case _ =>
      }
      suffixes(s(0)).withPrefix(s.substring(1))
    }
  }

  override def iterator: Iterator[(String, A)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((ch, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (ch +: s, v))


  def addOne(kv: (String, A)): this.type = {
    withPrefix(kv._1).value = Some(kv._2)
    this
  }

  def subtractOne(s: String): this.type = {
    if (s.isEmpty) {
      val prev = value;
      value = None;
      prev
    }
    else {
      suffixes.get(s(0)).flatMap(_.remove(s.substring(1)))
    }
    this
  }

  def map[B](f: ((String, A)) => (String, B)): PrefixMap[B] = strictOptimizedMap(PrefixMap.newBuilder, f)

  def flatMap[B](f: ((String, A)) => PrefixMap[B]): PrefixMap[B] = strictOptimizedFlatMap(PrefixMap.newBuilder, f)

  override def concat[B >: A](suffix: IterableOnce[(String, B)]): PrefixMap[B] = strictOptimizedConcat(suffix, PrefixMap.newBuilder[B])

  override def fromSpecific(coll: IterableOnce[(String, A)]): PrefixMap[A] = PrefixMap.from(coll)

  override def newSpecificBuilder: mutable.Builder[(String, A), PrefixMap[A]] = PrefixMap.newBuilder[A]

  override def clear(): Unit = suffixes = immutable.Map.empty

  override def className = "PrefixMap"

  override def empty: PrefixMap[A] = new PrefixMap
}

object PrefixMap {
  def empty[A] = new PrefixMap[A]

  def newBuilder[A]: mutable.Builder[(String, A), PrefixMap[A]] = new mutable.GrowableBuilder[(String, A), PrefixMap[A]](empty)

  def from[A](source: IterableOnce[(String, A)]): PrefixMap[A] = source match {
    case pm: PrefixMap[A] => pm
    case _ => (newBuilder ++= source).result()
  }

  def apply[A](t: (String, A)*) = from(t)

  //implicit conversion to Factory for a better interoperability with other collections
  implicit def toFactory[A](self: this.type): Factory[(String, A), PrefixMap[A]] = new Factory[(String, A), PrefixMap[A]] {
    override def fromSpecific(it: IterableOnce[(String, A)]): PrefixMap[A] = self.from(it)

    override def newBuilder: mutable.Builder[(String, A), PrefixMap[A]] = self.newBuilder[A]
  }
}

object PrefixMapApp extends App {
  val m = PrefixMap("abc" -> 0, "abde" -> 1, "al" -> 2, "all" -> 3, "xy" -> 4)
  println(m)
  println(m.withPrefix("a"))
  println(m.withPrefix("ab"))
  println(m.withPrefix("al"))
  println(m.withPrefix("all"))
  println(m.withPrefix("z"))

  //println(m.withPrefix("ab"))
  //println(m.withPrefix("abd"))
}

//PrefixMapSD(abc -> 0, abde -> 1, al -> 2, all -> 3, xy -> 4)
//PrefixMapSD(bc -> 0, bde -> 1, l -> 2, ll -> 3)
//PrefixMapSD(c -> 0, de -> 1)
//PrefixMapSD( -> 2, l -> 3)
//PrefixMapSD( -> 3)


//PrefixMap(abc->0,all->3,al->2,xy->4,abde->1)
//PrefixMap(bc->0,bde->1,ll->3,l->2)
//PrefixMap(c->0,de->1)
//PrefixMap(l->3,->2)
//PrefixMap(->3)
