object Solution {
    class BinomHeap[T](lt: (T, T) => Boolean) {
        def min(a: Option[T], b: Option[T]): Option[T] =
            (a, b) match {
                case (None, None) => None
                case (None, _) => b
                case (_, None) => a
                case (Some(a), Some(b)) =>
                    if (lt(a, b))
                        Some(a)
                    else
                        Some(b)
            }

        case class Node(val rank: Int, val x: T, val ts: List[Node]) {
            val size = 1 << rank

            override def toString =
                "Tree (" ++ rank.toString ++ "): " ++ x.toString ++ " [" ++ ((ts map (_.toString)) mkString ", ") ++ "]"

            def link(other: Node) = {
                require(rank == other.rank)

                if (lt(x, other.x))
                    new Node(rank + 1, x, other :: ts)
                else
                    new Node(rank + 1, other.x, this :: other.ts)
            }
        }

        class Heap(xxmin: => Option[T], val size: Int, val ts: List[Node]) {
            lazy val xmin: Option[T] = xxmin

            override def toString =
                "Heap:\n" ++ ((ts map (x => "\t" ++ x.toString)) mkString "\n") ++ "\n"

            def isEmpty = size == 0

            def insert(x: T): Heap =
                insTree(new Node(0, x, Nil))

            def merge(other: Heap) =
                new Heap(min(xmin, other.xmin), size + other.size, mergeAux(other).ts)

            def deleteMin = {
                val (t @ Node(_, x, ts1), h2) = removeMinTree
                mkHeap(t.size - 1, ts1.reverse) merge h2
            }

            private def mkHeap(sz: Int, ts: List[Node]): Heap =
                new Heap((new Heap(None, sz, ts)).findMin, sz, ts)

            private def findMin = {
                val (t, _) = removeMinTree
                Some(t.x)
            }

            @annotation.tailrec
            private def insTree(t: Node): Heap =
                ts match {
                    case Nil => new Heap(Some(t.x), size + t.size, List(t))
                    case t0 :: ts0 =>
                        if (t.rank < t0.rank)
                            new Heap(min(Some(t.x), xmin), size + t.size, t :: ts)
                        else {
                            require(t.rank == t0.rank)
                            mkHeap(size - t0.size, ts0) insTree (t link t0)
                        }
                }

            private def mergeAux(other: Heap): Heap =
                (ts, other.ts) match {
                    case (_, Nil) => this
                    case (Nil, _) => other
                    case (t1 :: ts1, t2 :: ts2) =>
                        if (t1.rank < t2.rank)
                            mkHeap(size + other.size, t1 :: (mkHeap(size - t1.size, ts1) mergeAux other).ts)
                        else if (t2.rank < t1.rank)
                            mkHeap(size + other.size, t2 :: (mergeAux(mkHeap(other.size - t2.size, ts2))).ts)
                        else
                            (mkHeap(size - t1.size, ts1) mergeAux mkHeap(size - t2.size, ts2)) insTree (t1 link t2)
                }

            private def removeMinTree: (Node, Heap) = {
                ts match {
                    case List(t) => (t, new Heap(None, 0, Nil))
                    case t :: ts => {
                        val (t0, h0) = mkHeap(size - t.size, ts).removeMinTree
                        if (lt(t.x, t0.x))
                            (t, mkHeap(size - t.size, ts))
                        else
                            (t0, mkHeap(t.size + h0.size, t :: h0.ts))
                    }
                    case _ => (ts.head, this)
                }
            }
        }

        def empty: Heap = new Heap(None, 0, Nil)
    }

    class RunningMedian(val l: BinomHeap[Int]#Heap, val r: BinomHeap[Int]#Heap) {
        require(l.size == r.size || l.size == r.size + 1)

        def med = l.xmin

        def add(x: Int) = {
            if (l.size == 0 || x <= l.xmin.get) {
                val nl = l insert x
                if (nl.size == r.size + 2) {
                    val x0 = nl.xmin
                    val nl0 = nl.deleteMin
                    new RunningMedian(nl0, r insert x0.get)
                }
                else
                    new RunningMedian(nl, r)
            }
            else {
                val nr = r insert x
                if (nr.size > l.size) {
                    val x0 = nr.xmin
                    val nr0 = nr.deleteMin
                    new RunningMedian(l insert x0.get, nr0)
                }
                else
                    new RunningMedian(l, nr)
            }
        }
    }

    @annotation.tailrec
    def loop(n: Int, lim: Int, rm: RunningMedian, store: collection.immutable.Vector[RunningMedian]): Unit = {
        if (n > lim)
            ()
        else {
            val x = readLine.toInt
            if (x > 0) {
                val rm0 = rm add x
                println(rm0.med.get)
                loop(n + 1, lim, rm0, store :+ rm0)
            }
            else {
                val rm0 = store(n + x - 1)
                println(rm0.med.get)
                loop(n + 1, lim, rm0, store :+ rm0)
            }
        }
    }

    def main(args: Array[String]) = {
        val rm = new RunningMedian(new BinomHeap[Int](_ > _).empty, new BinomHeap[Int](_ < _).empty)
        val n = readLine.toInt
        loop(1, n, rm, collection.immutable.Vector())
    }
}
