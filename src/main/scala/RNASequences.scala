import collection.immutable.{IndexedSeq, IndexedSeqOps, StrictOptimizedSeqOps}
import scala.collection.{AbstractIterable, AbstractIterator, SpecificIterableFactory, mutable}
// RNA strands, which are sequences of bases A (adenine), U (uracil), G (guanine), and C (cytosine)

abstract class Base

case object A extends Base

case object U extends Base

case object G extends Base

case object C extends Base

object Base {
  def fromInt: Int => Base = Array(A, U, G, C)

  def toInt: Base => Int = Map(A -> 0, U -> 1, G -> 2, C -> 3)
}

final class RNA private(val groups: Array[Int], val length: Int)
  extends IndexedSeq[Base]
    with IndexedSeqOps[Base, IndexedSeq, RNA]
    with StrictOptimizedSeqOps[Base, IndexedSeq, RNA] {
  rna =>

  import RNA._

  def this(length: Int) = this(Array.ofDim((length + RNA.N - 1) / RNA.N), length)

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException

    Base.fromInt((groups(idx / N) >> idx % N * S) & M)
  }

  def update(idx: Int, base: Base) = {
    groups(idx / N) = groups(idx / N) | (Base.toInt(base) << idx % N * S)
  }

  override def toString: String = {
    val bases = for {
      group <- groups
      shift <- 0 to N - 1
    } yield Base.fromInt((group >> (shift * S)) & M)

    bases.take(length).mkString("RNA(", ",", ")")
  }

  override protected def fromSpecific(coll: IterableOnce[Base]): RNA = RNA.fromSpecific(coll)

  override protected def newSpecificBuilder: mutable.Builder[Base, RNA] = RNA.newBuilder

  override protected def className: String = "RNA"

  override def empty: RNA = RNA.empty

  def concat(suffix: IterableOnce[Base]) = strictOptimizedConcat(suffix, newSpecificBuilder)

  @inline final def ++(suffix: IterableOnce[Base]) = concat(suffix)

  def appended(base: Base): RNA = (newSpecificBuilder ++= this += base).result()

  def appendedAll(suffix: Iterable[Base]): RNA = strictOptimizedConcat(suffix, newSpecificBuilder)

  def prepended(base: Base): RNA = (newSpecificBuilder += base ++= this).result()

  def prependedAll(prefix: Iterable[Base]): RNA = (newSpecificBuilder ++= prefix ++= this).result()

  def map(f: Base => Base): RNA = strictOptimizedMap(newSpecificBuilder, f)

  def flatMap(f: Base => IterableOnce[Base]): RNA = strictOptimizedFlatMap(newSpecificBuilder, f)

  override def iterator: Iterator[Base] = new AbstractIterator[Base] {
    var i = 0
    var b = 0

    override def hasNext: Boolean = i < rna.length

    override def next(): Base = {
      b = if (i % N == 0) groups(i / N) else b >>> S
      i += 1
      Base.fromInt(b & M)
    }
  }
}

object RNA extends SpecificIterableFactory[Base, RNA] {
  private val S = 2 //bits required to represent one Base
  private val N = 32 / S //number of bases per integer

  // bit mask to isolate a group
  private val M = (1 << S) - 1

  def fromSeq(buf: collection.Seq[Base]) = {
    val length = buf.length
    val groups = new Array[Int]((length + N - 1) / N)
    for (index <- 0 until buf.length)
      groups(index / N) |= Base.toInt(buf(index)) << (index % N * S)
    new RNA(groups, length)
  }

  override def empty: RNA = fromSeq(Seq.empty)

  override def newBuilder: mutable.Builder[Base, RNA] = mutable.ArrayBuffer.newBuilder[Base].mapResult(fromSeq)

  override def fromSpecific(it: IterableOnce[Base]): RNA = it match {
    case seq: collection.Seq[Base] => fromSeq(seq)
    case _ => fromSeq(mutable.ArrayBuffer.from(it))
  }
}

object Test extends App {
  val rna1 = RNA.fromSeq(Seq(G, G, U, U, C, C, A, G, C))
  val rna2 = RNA(A, U, G, G, C)
  println(rna1)
  println(rna1.filter(_ != U))
  println(rna2)
  println(rna1 ++ rna2)
  println(rna2.map(x => x))
  println(rna2.map(_.toString))
  println(rna2 :+ G)
  println(rna2 ++ Seq(G, G, G))
  println(rna2 ++ RNA.fromSeq(Seq(A, G, C, C, C, C)))
}
