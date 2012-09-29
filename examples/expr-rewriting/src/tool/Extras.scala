package ee.cyber.simplicitas.exprewriting

import ee.cyber.simplicitas.{CommonNode, TerminalNode}

case class BinOp(left: Expr, op: String, right: Expr)
        extends CommonNode with Expr {
    def childrenNames = Array("left", "op", "right")
}

object Extras {
    def makeExpr(left: Expr, op: TerminalNode, right: Expr) =
        if (right eq null)
            left
        else
            BinOp(left, op.text, right)
}