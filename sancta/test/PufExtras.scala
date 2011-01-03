package puf.spl

import ee.cyber.simplicitas.CommonNode

case class Unary(op: UnaryOp.Type, arg: Expr) extends Expr {
    def childrenNames = Array("op", "arg")
}

object PufExtras {
    def makeBinaryOp(op: String, left: Expr, right: Expr) = {
        left
    }

    def makeBinaryOp(args: List[Expr], ops: List[CommonNode]) = {
        args(0)
    }

    def makeBinaryOp(op: BinaryOp.Type, args: List[Expr]) = {
        new Num("42")
    }

    def makeBinaryOp(op: BinaryOp.Type, left: Expr, right: Expr) = {
        new Num("42")
    }
}

object BinaryOp extends Enumeration {
    type Type = Value

    val Cons = Value
    val And = Value
    val Or = Value
}

object UnaryOp extends Enumeration {
    type Type = Value

    val Not = Value
    val Neg = Value
}
