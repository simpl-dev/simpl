package ee.cyber.simplicitas.grammartest.puf

import ee.cyber.simplicitas.{TerminalNode, CommonNode}

case class Unary(op: UnaryOp.Type, arg: Expr) extends Expr {
    def childrenNames = Array("op", "arg")
}

case class Binary(op: BinaryOp.Type, left: Expr, right: Expr) extends Expr {
    def childrenNames = Array("op", "left", "right")
}

case class ListNil extends Expr {
    def childrenNames = Array.empty
}

object PufExtras {
    type WithText = {
        def text: String
    }

    def makeBinaryOp(op: TerminalNode, left: Expr, right: Expr) =
        if (right ne null)
            Binary(BinaryOp.withName(op.text), left, right)
        else
            left

    def makeBinaryOp(args: List[Expr], ops: List[WithText]) = {
        def loop(left: Expr, right: List[Expr],
                 ops: List[BinaryOp.Type]): Expr =
            (right, ops) match {
                case (rh :: rt, oh :: ot) =>
                    loop(
                        Binary(oh, left, rh).setStart(left).setEnd(rh),
                        rt, ot)
                case (Nil, Nil) =>
                    left
            }

        loop(args.head, args.tail,
            ops.map((op: WithText) => BinaryOp.withName(op.text)))
    }

    def makeBinaryOp(op: BinaryOp.Type, args: List[Expr]) = {
        splitLeftAssoc(args, op)
    }

    def makeBinaryOp(op: BinaryOp.Type, left: Expr, right: Expr) =
        if (right ne null)
            Binary(op, left, right)
        else
            left

    implicit def binaryCons(op: BinaryOp.Type) =
        (x1: Expr, x2: Expr) =>
            Binary(op, x1, x2).setStart(x1).setEnd(x2)

    def splitLeftAssoc(lst: List[Expr], cons: (Expr, Expr) => Expr): Expr = {
        lst match {
            case h :: t =>
                t.foldLeft(h)(cons)
            case Nil =>
                throw new Exception("Invalid node: " + lst)
        }
    }
}

object BinaryOp extends Enumeration {
    type Type = Value

    val Plus = Value("+")
    val Minus = Value("-")
    val Times = Value("*")
    val Div = Value("/")
    val Mod = Value("%")
    val LessThan = Value("<")
    val LessEqual = Value("<=")
    val GreaterThan = Value(">")
    val GreaterEqual = Value(">=")
    val Equals = Value("==")
    val NotEquals = Value("/=")
    val And = Value("&&")
    val Or = Value("||")
}

object UnaryOp extends Enumeration {
    type Type = Value

    val Neg, Not = Value
}