object Solution {
    def sat(mx: BigInt, m: Int, xs: List[(BigInt, BigInt)]) =
        ((xs map (x => x._1 + (m - 1) * x._2)) take m).sorted.sum <= mx

    @annotation.tailrec
    def solve(mx: BigInt, a: Int, b: Int, xs: List[(BigInt, BigInt)]): Int =
        if (a == b)
            a
        else {
            val m: Int = (a + b + 1) / 2
            if (sat(mx, m, xs))
                solve(mx, m, b, xs)
            else
                solve(mx, a, m - 1, xs)
        }

    def main(args: Array[String]) = {
        val ps = readLine.split(" ").map(x => BigInt(x))
        val as = readLine.split(" ").map(x => BigInt(x))
        val hs = readLine.split(" ").map(x => BigInt(x))
        val xs = (as zip hs) sortWith ((x, y) => (x._2 < y._2) || (x._2 == y._2 && x._1 < y._1))
        println(solve(ps(1), 0, ps(0).toInt, xs.toList))
    }
}
