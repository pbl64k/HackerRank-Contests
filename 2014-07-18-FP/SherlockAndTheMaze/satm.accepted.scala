object Solution {
    val mm: Long = 1000000007L

    sealed class F(n: Int, m: Int, k: Int) {
        lazy val f = ff(n, m, k)
    }

    val dpa =
        (for {
            n <- 1 to 100
            m <- 1 to 100
            k <- 0 to 100
        } yield (new F(n, m, k))).toArray

    def dp(ix: (Int, Int, Int)): F =
        dpa(ix._3 + (ix._2 - 1) * 101 + (ix._1 - 1) * 10100)

    def g(n: Int, m: Int, k: Int): Long = {
        if (n > 0 && m > 0 && k >= 0)
            dp((n, m, k)).f
        else
            0
    }

    def ff(n: Int, m: Int, k: Int): Long = {
        (n, m, k) match {
            case (_, 1, 0) => 1L
            case (1, 1, _) => 0L
            case (_, 1, _) => 0L
            case (1, _, _) => 0L
            case (n, m, k) => (g(n - 1, m, k) + g(m, n - 1, k - 1)) % mm
        }
    }

    def main(args: Array[String]) = {
        val t = readLine.toInt
        for (ix <- 1 to t) {
            val (n :: m :: k :: _) = readLine.split(" ").map(_.toInt).toList
            if (n == 1 && m == 1)
                println("1")
            else {
                val ss = for (jx <- 0 to k) yield (dp((n, m, jx)).f + dp((m, n, jx)).f)
                println((ss foldLeft 0L)((a, b) => (a + b) % mm))
            }
        }
    }
}
