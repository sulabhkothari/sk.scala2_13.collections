import scala.collection._
import scala.collection.mutable.{ GrowableBuilder, Builder }

class PrefixMapSD[A]
  extends mutable.Map[String, A]
    with mutable.MapOps[String, A, mutable.Map, PrefixMapSD[A]]
    with StrictOptimizedIterableOps[(String, A), mutable.Iterable, PrefixMapSD[A]] {

  private var suffixes: immutable.Map[Char, PrefixMapSD[A]] = immutable.Map.empty
  private var value: Option[A] = None

  def get(s: String): Option[A] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  def withPrefix(s: String): PrefixMapSD[A] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  def iterator: Iterator[(String, A)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def addOne(kv: (String, A)): this.type = {
    withPrefix(kv._1).value = Some(kv._2)
    this
  }

  def subtractOne(s: String): this.type  = {
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))
    this
  }

  // Overloading of transformation methods that should return a PrefixMapSD
  def map[B](f: ((String, A)) => (String, B)): PrefixMapSD[B] =
    strictOptimizedMap(PrefixMapSD.newBuilder, f)
  def flatMap[B](f: ((String, A)) => IterableOnce[(String, B)]): PrefixMapSD[B] =
    strictOptimizedFlatMap(PrefixMapSD.newBuilder, f)

  // Override `concat` and `empty` methods to refine their return type
  override def concat[B >: A](suffix: IterableOnce[(String, B)]): PrefixMapSD[B] =
    strictOptimizedConcat(suffix, PrefixMapSD.newBuilder)
  override def empty: PrefixMapSD[A] = new PrefixMapSD

  // Members declared in scala.collection.mutable.Clearable
  override def clear(): Unit = suffixes = immutable.Map.empty
  // Members declared in scala.collection.IterableOps
  override protected def fromSpecific(coll: IterableOnce[(String, A)]): PrefixMapSD[A] = PrefixMapSD.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[(String, A), PrefixMapSD[A]] = PrefixMapSD.newBuilder

  override def className = "PrefixMapSD"
}

object PrefixMapSD {
  def empty[A] = new PrefixMapSD[A]

  def from[A](source: IterableOnce[(String, A)]): PrefixMapSD[A] =
    source match {
      case pm: PrefixMapSD[A] => pm
      case _ => (newBuilder ++= source).result()
    }

  def apply[A](kvs: (String, A)*): PrefixMapSD[A] = from(kvs)

  def newBuilder[A]: mutable.Builder[(String, A), PrefixMapSD[A]] =
    new mutable.GrowableBuilder[(String, A), PrefixMapSD[A]](empty)

  import scala.language.implicitConversions

  implicit def toFactory[A](self: this.type): Factory[(String, A), PrefixMapSD[A]] =
    new Factory[(String, A), PrefixMapSD[A]] {
      def fromSpecific(it: IterableOnce[(String, A)]): PrefixMapSD[A] = self.from(it)
      def newBuilder: mutable.Builder[(String, A), PrefixMapSD[A]] = self.newBuilder
    }

}

object PMSD extends App {
  val m = PrefixMapSD("abc" -> 0, "abde" -> 1, "al" -> 2, "all" -> 3, "xy" -> 4)
  println(m)
  println(m.withPrefix("a").toString())
  println(m.withPrefix("ab"))
  println(m.withPrefix("al"))
  println(m.withPrefix("all"))
  println(m.withPrefix("z"))
}
