import annotation.tailrec

object Solution {
    sealed abstract class Op
    case object IncPtr extends Op
    case object DecPtr extends Op
    case object IncByte extends Op
    case object DecByte extends Op
    case object Outp extends Op
    case object Inp extends Op
    case class Loop(val body: List[Op]) extends Op
    case object Pool extends Op

    sealed class State(val opnum: Int, val ix: Int,
            val mem: collection.immutable.Vector[Byte],
            val stdin: List[Byte])

    val maxIters = 100000

    @tailrec
    def eval(program: List[Op], state: State, inner: Boolean): (Boolean, State) =
    program match {
        case Nil => {
            if (inner)
                (state.mem(state.ix) != 0, state)
            else
                (false, state)
        }
        case (op :: ops) => {
            val newState = evalOp(op, state)
            eval(ops, newState, inner)
        }
    }

    @tailrec
    def evalOp(op: Op, state: State): State = {
        if (state.opnum >= maxIters) {
            println("\nPROCESS TIME OUT. KILLED!!!")
            System.exit(0)
            state
        } else op match {
            case IncPtr => new State(state.opnum + 1, state.ix + 1, state.mem, state.stdin)
            case DecPtr => new State(state.opnum + 1, state.ix - 1, state.mem, state.stdin)
            case IncByte => new State(state.opnum + 1, state.ix,
                    state.mem.updated(state.ix, (state.mem(state.ix) + (1.toByte)).toByte),
                    state.stdin)
            case DecByte => new State(state.opnum + 1, state.ix,
                    state.mem.updated(state.ix, (state.mem(state.ix) - (1.toByte)).toByte),
                    state.stdin)
            case Outp => {
                print(state.mem(state.ix).toChar)
                new State(state.opnum + 1, state.ix, state.mem, state.stdin)
            }
            case Inp => new State(state.opnum + 1, state.ix,
                    state.mem.updated(state.ix, state.stdin.head),
                    state.stdin.tail)
            case Loop(body) => {
                if (state.mem(state.ix) == 0)
                    new State(state.opnum + 2, state.ix, state.mem, state.stdin)
                else {
                    val (repeat, newState) = eval(body, new State(state.opnum + 1,
                            state.ix, state.mem, state.stdin), true)
                    if (repeat)
                        evalOp(op, newState)
                    else
                        newState
                }
            }
            case Pool => new State(state.opnum + 1, state.ix, state.mem, state.stdin)
        }
    }

    def parse(code: String): List[Op] = {
        @tailrec
        def aux(code: String, acc: List[Op]): (List[Op], String) = {
            if (code.isEmpty)
                (acc.reverse, code)
            else
                code.head match {
                    case '>' => aux(code.tail, IncPtr :: acc)
                    case '<' => aux(code.tail, DecPtr :: acc)
                    case '+' => aux(code.tail, IncByte :: acc)
                    case '-' => aux(code.tail, DecByte :: acc)
                    case '.' => aux(code.tail, Outp :: acc)
                    case ',' => aux(code.tail, Inp :: acc)
                    case '[' => {
                        val (ops, rem) = aux2(code.tail, Nil)
                        aux(rem, Loop(ops) :: acc)
                    }
                    case ']' => ((Pool :: acc).reverse, code.tail)
                    case _ => aux(code.tail, acc)
                }
        }
        def aux2(code: String, acc: List[Op]) = aux(code, acc)
        aux(code, Nil)._1
    }

    def main(args: Array[String]) {
        val l = readLine.split(" ").map(_.toInt)
        val stdin = readLine.toList.map(_.toByte)
        val program = (for (n <- 0.to(l(1))) yield readLine).toList mkString ""
        val ast = parse(program)
        eval(ast, new State(0, 0,
                collection.immutable.Vector.fill(maxIters)(0.toByte), stdin), false)
    }
}
