import scala.collection.immutable.Map

object Solution {
    def comb(xs: List[Int]) = {
        def g(m: Map[Int, Int], x: Int) =
            (m get x) match {
                case None => m updated (x, 1)
                case Some(n) => m updated (x, n + 1)
            }
        (xs foldLeft (Map.empty[Int, Int]))(g)
    }

    def f0(xs: List[Int], k: Int): List[Int] = {
        def g(acc: (List[Int], Map[Int, Int]), x: Int) =
            (acc._2 get x) match {
                case None => acc
                case Some(n) =>
                    if (n >= k)
                        (x :: acc._1, acc._2 - x)
                    else
                        (acc._1, acc._2 - x)
            }
        (xs.foldLeft (List[Int](), comb(xs)))(g)._1.reverse
    }

    def f(xs: List[Int], k: Int) =
        f0(xs, k) match {
            case Nil => "-1"
            case xs => (xs map (_.toString)) mkString " "
        }

    def main(args: Array[String]) = {
        val t = readLine.toInt
        for (ix <- 0 until t) {
            val p = readLine.split(" ").map(_.toInt).toList
            val xs = readLine.split(" ").map(_.toInt).toList
            println(f(xs, p(1)))
        }
    }
}
