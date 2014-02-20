import collection.immutable.Map
import collection.immutable.TreeMap

object Solution {
    val f = 1000000007

    sealed class Factors(n: Long) {
        lazy val list = factors(n + 1)
    }

    val fs: collection.immutable.Vector[Factors] =
            collection.immutable.Vector.tabulate(10000)((x: Int) => new Factors(x.toLong))

    def factors(n: Long): List[Long] = {
        @annotation.tailrec
        def aux(d: Long): List[Long] = {
            if (d >= n)
                List(n)
            else if (n % d == 0)
                (fs((d - 1).toInt).list ++ fs(((n / d) - 1).toInt).list)
            else
                aux(d + 1)
        }
        aux(2)
    }

    def pmap(xs: List[Long]): Map[Long, Long] = {
        def aux0(x: Long, acc: Map[Long, Long]): Map[Long, Long] = {
            @annotation.tailrec
            def aux00(ps: List[Long], acc: Map[Long, Long]): Map[Long, Long] =
                if (ps.isEmpty)
                    acc
                else
                    aux00(ps.tail, acc.updated(ps.head, acc(ps.head) + 1))
            aux00(fs((x - 1).toInt).list, acc)
        }
        @annotation.tailrec
        def aux(xs: List[Long], acc: Map[Long, Long]): Map[Long, Long] =
            if (xs.isEmpty)
                acc
            else
                aux(xs.tail, aux0(xs.head, acc))
        aux(xs, new TreeMap[Long, Long]().withDefaultValue(0))
    }

    def gcd(ns: Map[Long, Long], ms: Map[Long, Long]): Long = {
        @annotation.tailrec
        def aux(ns: List[(Long, Long)], ms: Map[Long, Long], acc: Long): Long =
            if (ns.isEmpty)
                acc
            else if (ns.head._2 == 0)
                aux(ns.tail, ms, acc)
            else {
                val pw = math.min(ns.head._2, ms(ns.head._1))
                val p = if (pw > 0) ns.head._1 else 1
                aux((ns.head._1, math.max(pw - 1, 0)) :: ns.tail, ms, (acc * p) % f)
            }
        aux(ns.toList, ms, 1)
    }

    def main(args: Array[String]) {
        readLine
        val ns = readLine.split(" ").map(_.toLong).toList
        readLine
        val ms = readLine.split(" ").map(_.toLong).toList
        println(gcd(pmap(ns), pmap(ms)))
    }
}
