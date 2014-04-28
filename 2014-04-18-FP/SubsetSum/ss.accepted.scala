object Solution {
    sealed abstract class STree
    case object Leaf extends STree
    case class Node(l: STree, n: Long, s: Long, r: STree) extends STree

    def mktree(n: Long, xs: List[(Long, Long)]): (STree, List[(Long, Long)]) = (n, xs) match {
        case (0, xs) => (Leaf, xs)
        case (1, (n, s)::xs) => (Node(Leaf, n, s, Leaf), xs)
        case (n, xs) => {
            val midp = (n + 1) / 2
            val (l, ((n1, s1)::xs1)) = mktree(midp - 1, xs)
            val (r, xs2) = mktree(n - midp, xs1)
            (Node(l, n1, s1, r), xs2)
        }
    }

    def tfind(t: STree, x: Long): Long = (t, x) match {
        case (Leaf, _) => -1
        case (Node(l, n, s, r), x) => 
            if (x == s)
                n
            else if (x < s) {
                val l1 = tfind(l, x)
                if (l1 == -1)
                    n
                else
                    l1
            }
            else
                tfind(r, x)
    }

    def tst(t: STree): Unit = {
        val s = readLine.toLong
        println(tfind(t, s))
    }

    def main(args: Array[String]) = {
        readLine
        val xs = readLine.split(" ").map(_.toLong).sortWith(_ > _)
        def f(z: (Long, List[Long]), x: Long): (Long, List[Long]) =
            (z._1 + x, (z._1 + x)::(z._2))
        val (_, ys) = (xs foldLeft (0L, List[Long]()))(f)
        val y0s = (1L to ys.length.toLong).toList zip (ys.reverse)
        val (tree, _) = mktree(y0s.length.toLong, y0s)
        val t = readLine.toLong
        for (ix <- 0L until t) {
            tst(tree)
        }
    }
}
