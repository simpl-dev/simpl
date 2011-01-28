package ee.cyber.simplicitas.puf

// This file contains helper classes and methods that are called by the
// grammar for assembling the AST.

import ee.cyber.simplicitas.{TerminalNode, CommonNode}

/** Umbrella class for both of the unary operators (negation and not). */
case class Unary(op: UnaryOp.Type, arg: Expr) extends Expr {
    def childrenNames = Array("op", "arg")
}

object UnaryOp extends Enumeration {
    type Type = Value

    val Neg, Not = Value
}

/** AST class for all the binary operators (except Cons). */
case class Binary(op: BinaryOp.Type, left: Expr, right: Expr) extends Expr {
    def childrenNames = Array("op", "left", "right")
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

/** The empty list literal. */
case class ListNil() extends Expr {
    def childrenNames = Array.empty
}

/** IdLeft will replace FunLeft. All the
 * FunDecl(FunLeft("foo", List("bar", "baz")), bag) AST nodes are replaced with
 * FunDecl(IdLeft("foo"), FunExpr(List("bar", "baz"), bag)
 *
 * Thus, there is always single identifier in the left side of the
 * equals sign.
 */
case class IdLeft(id: Id) extends DeclLeft {
    def childrenNames = Array("id")
}

/** This object contains methods that are used co compose more convenient
  * AST. */
object PufExtras {
    type WithText = {
        def text: String
    }

    /** Removes the syntactic sugar from let expressions:
     * let foo bar baz = bag; in ...
     * is replaced with:
     * let foo = fn bar baz -> bag; in ...
     */
    def makeFunDecl(left: DeclLeft, expr: Expr) = left match {
        case FunLeft(id :: Nil) =>
            FunDecl(IdLeft(id).setLocation(id), expr)
        case FunLeft(fun :: params) =>
            val lambda = FunExpr(params, expr)
            lambda.setStart(params.head)
            lambda.setEnd(expr)

            FunDecl(IdLeft(fun).setLocation(fun), lambda)
        case _ =>
            throw new IllegalArgumentException("Invalid fundecl: " + left)
    }

    /** If both left and right parameters are present, create Binary() object.
     */
    def makeBinaryOp(op: TerminalNode, left: Expr, right: Expr) =
        if (right ne null)
            Binary(BinaryOp.withName(op.text), left, right)
        else
            left

    /** Take list of arguments and operators (the operators list should
     * contain one less item than arguments) and create nested Binary
     * objects.
     * The expression 1 + 2 - 3 + 4 arrives as
     * args=List(Num(1), Num(2), Num(3), Num(4))
     * ops=List(PlusOp("+"), MinusOp("-"), PlusOp("+"))
     */
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
                case _ =>
                    throw new Exception("Cannot happen")
            }

        loop(args.head, args.tail,
            ops.map((op: WithText) => BinaryOp.withName(op.text)))
    }

    /** Convert list of terms to Binary operators. */
    def makeBinaryOp(op: BinaryOp.Type, args: List[Expr]) = {
        splitLeftAssoc(args, op)
    }

    /** If both left and right parameters are present, create Binary() object.
     */
    def makeBinaryOp(op: BinaryOp.Type, left: Expr, right: Expr) =
        if (right ne null)
            Binary(op, left, right)
        else
            left

    implicit def binaryCons(op: BinaryOp.Type) =
        (x1: Expr, x2: Expr) =>
            Binary(op, x1, x2).setStart(x1).setEnd(x2)

    private def splitLeftAssoc(
              lst: List[Expr],
              cons: (Expr, Expr) => Expr): Expr = {
        lst match {
            case h :: t =>
                t.foldLeft(h)(cons)
            case Nil =>
                throw new Exception("Invalid node: " + lst)
        }
    }
}
