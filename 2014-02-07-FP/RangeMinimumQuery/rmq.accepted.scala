object Solution /* extends App */ {
    sealed abstract class Mintree
    case class Leaf(m: Int, a: Int) extends Mintree
    case class Node(m: Int, a: Int, b: Int, l: Mintree, r: Mintree) extends Mintree

    def tmin(t: Mintree): Int = t match {
        case Leaf(m, _) => m
        case Node(m, _, _, _, _) => m
    }

    def buildtree(xs: List[Int]): Mintree = {
        def aux(a: Int, b: Int, xs: List[Int]): (List[Int], Mintree) =
            if (a == b)
                (xs.tail, Leaf(xs.head, a))
            else {
                val c = a + (b - a) / 2
                val (xs1, left) = aux(a, c, xs)
                val (xs2, right) = aux(c + 1, b, xs1)
                (xs2, Node(if (tmin(left) < tmin(right)) tmin(left) else tmin(right), a, b, left, right))
            }                
        aux(0, xs.length - 1, xs)._2
    }

    def mm(a: Option[Int], b: Option[Int]): Option[Int] = (a, b) match {
        case (a, None) => a
        case (None, a) => a
        case (Some(a), Some(b)) => Some(if (a < b) a else b)
    }

    def findmin(t: Mintree, a: Int, b: Int): Option[Int] = t match {
        case Leaf(m, at) =>
            if (a <= at && at <= b)
                Some(m)
            else
                None
        case Node(m, at, bt, l, r) =>
            if (a <= at && bt <= b)
                Some(m)
            else
                if (b < at || bt < a)
                    None
                else
                    mm(findmin(l, a, b), findmin(r, a, b))
    }

    /* override */ def main(args: Array[String]) {
        val l = readLine.split(" ").map(_.toInt)
        val xs = readLine.split(" ").map(_.toInt).toList
        val t = buildtree(xs)
        for (ix <- 0.until(l(1))) {
            val r = readLine.split(" ").map(_.toInt)
            println(findmin(t, r(0), r(1)).head)
        }
    }
}
