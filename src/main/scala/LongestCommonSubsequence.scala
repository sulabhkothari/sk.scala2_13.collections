import scala.collection.mutable.ListBuffer

object LongestCommonSubsequence {

  implicit class Lcs[A](indexedSeq: IndexedSeq[A]) {
    def lcs(indexedSeq2: IndexedSeq[A]): List[A] = {
      val (m, n, biggerSeq, smallerSeq) =
        if (indexedSeq.length > indexedSeq2.length) (indexedSeq.length, indexedSeq2.length, indexedSeq, indexedSeq2)
        else (indexedSeq2.length, indexedSeq.length, indexedSeq2, indexedSeq)
      val arr = Array.ofDim[Int](n + 1, m + 1)
      for {
        i <- 1 until n + 1
        j <- 1 until m + 1
      } {
        arr(i)(j) = Math.max(arr(i - 1)(j), arr(i)(j - 1))
        if (biggerSeq(j - 1) == smallerSeq(i - 1)) {
          arr(i)(j) = arr(i - 1)(j - 1) + 1
        }
      }

      //            arr.foreach(x => {
      //              println("")
      //              x.foreach(y => print(y + "  "))
      //            })

      var x = m
      var y = n
      val listBuffer = ListBuffer.empty[A]
      while (x > 0 && y > 0 && arr(y)(x) > 0) {
        if (arr(y)(x) == arr(y)(x - 1)) {
          x -= 1
        }
        else if (arr(y - 1)(x) == arr(y)(x)) {
          y -= 1
        }
        else {
          listBuffer += biggerSeq(x - 1)
          x -= 1
        }
      }

      listBuffer.reverse.toList
    }
  }

}


object TestLcs extends App {
  val str2 = "abcdefgytipljkzzzmmmmmmmmmm".toIndexedSeq
  val str1 = "jkabyxdftljjjjjjjjkabc".toIndexedSeq

  import LongestCommonSubsequence._
  //val a = new Lcs[Char](str1)
  println(str1.lcs(str2))
  val rna2 = RNA(A, G, G, G, U, C, A, G, C, C, C, A, A, A, G, G, U, U, U)
  val rna1 = RNA(G, A, U, C, U, U, U, A, G, G, G, G, G, A, A, A, U)
  println(rna1.lcs(rna2))
}
