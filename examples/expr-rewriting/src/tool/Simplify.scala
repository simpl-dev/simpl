package ee.cyber.simplicitas.exprewriting

import org.kiama.rewriting.Rewriter._

import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.simplicitas.CommonNode

object Simplify {
    val simplifyAdd = rule {
        case BinOp(Num("0"), "+", expr) => expr
        case BinOp(expr, "+", Num("0")) => expr
    }

    val simplifyMul = rule {
        case BinOp(Num("0"), "*", _) => Num("0")
        case BinOp(_, "*", Num("0")) => Num("0")
        case BinOp(Num("1"), "*", expr) => expr
        case BinOp(expr, "*", Num("1")) => expr
    }

    val simplifyStrat = bottomup(attempt(simplifyAdd <+ simplifyMul))

    def simplify(expr: Expr) {
        println("Before:\n" + prettyPrint(expr))

        val simplified = simplifyStrat(expr)

        println("After:\n" + prettyPrint(
            simplified.get.asInstanceOf[CommonNode]))
    }
}