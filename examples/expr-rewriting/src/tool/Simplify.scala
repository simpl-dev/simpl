package ee.cyber.simplicitas.exprewriting

import org.kiama.rewriting.Rewriter._

import ee.cyber.simplicitas.PrettyPrint._
import ee.cyber.simplicitas.CommonNode

object Simplify {
    val simplifyAdd = rule {
        case BinOp(Num("0"), "+", expr) => expr
        case BinOp(expr, "+", Num("0")) => expr
    }

    val simplifySub = rule {
        case BinOp(Num("0"), "-", expr) => expr
        case BinOp(expr, "-", Num("0")) => expr
    }

    val simplifyMul = rule {
        case BinOp(Num("0"), "*", _) => Num("0")
        case BinOp(_, "*", Num("0")) => Num("0")
        case BinOp(Num("1"), "*", expr) => expr
        case BinOp(expr, "*", Num("1")) => expr
    }

    val simplifyDiv = rule {
        case BinOp(Num("0"), "/", _) => Num("0")
        case BinOp(_, "/", Num("0")) => throw new Exception("Divide by zero")
        case BinOp(expr, "/", Num("1")) => expr
    }

    val evalConstants = rule {
        case BinOp(Num(left), "+", Num(right)) =>
            evalOp(left, _ + _, right)
        case BinOp(Num(left), "-", Num(right)) =>
            evalOp(left, _ - _, right)
        case BinOp(Num(left), "*", Num(right)) =>
            evalOp(left, _ * _, right)
        case BinOp(Num(left), "/", Num(right)) =>
            evalOp(left, _ / _, right)
    }

    def evalOp(left: String, op: (Int, Int) => Int, right: String) =
        Num(op(left.toInt, right.toInt).toString)

    val simplifyStrat = innermost(
            simplifyAdd
            <+ simplifySub
            <+ simplifyMul
            <+ simplifyDiv
            <+ evalConstants)

    def simplify(expr: Expr) {
        println("Before:\n" + prettyPrint(expr))

        val simplified = simplifyStrat(expr)

        println("After:\n" + prettyPrint(
            simplified.get.asInstanceOf[CommonNode]))
    }
}