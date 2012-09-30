package ee.cyber.simplicitas.exprewriting

import ee.cyber.simplicitas.{CommonNode, TerminalNode}

case class BinOp(left: Expr, op: String, right: Expr)
        extends CommonNode with Expr {
    def childrenNames = Array("left", "op", "right")
}

object Extras {
    def makeExpr(ops: List[TerminalNode], args: List[Expr]): Expr =
        args match {
            case h :: Nil =>
                h
            case l :: r :: t =>
                makeExpr(
                    ops.tail,
                    BinOp(l, ops.head.text, r) :: t)
            case _ =>
                // Just to make warning go away.
                throw new IllegalArgumentException();
        }
}