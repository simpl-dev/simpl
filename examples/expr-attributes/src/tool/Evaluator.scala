package ee.cyber.simplicitas.expattributes

import org.kiama.attribution.Attribution._

object Evaluator {
    val eval: Expr => Int =
        attr {
            case Num(s) => s.toInt
            case BinOp(l, "+", r) => eval(l) + eval(r)
            case BinOp(l, "-", r) => eval(l) - eval(r)
            case BinOp(l, "*", r) => eval(l) * eval(r)
            case BinOp(l, "/", r) => eval(l) / eval(r)
        }
}