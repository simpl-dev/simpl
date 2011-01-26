package ee.cyber.simplicitas.puf

import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.{CommonNode, SourceMessage}

object PufChecker {
    def process(tree: Program) = {
        val ctx = new PufChecker(tree)
        ctx.process()
        ctx.errors
    }
}

class PufChecker(tree: Program) {
    type Env = Map[String, CommonNode]

    val errors = ArrayBuffer[SourceMessage]()
    def process() {
        // TODO: doc comments.

        resolveLinks(tree, Map.empty)
    }

    def resolveLinks(node: CommonNode, env: Env) {
        node match {
            case Program(_, decls) =>
                processLetrec(decls, env)
            case FunExpr(params, expr) => ()
            case ConsExpr(left, right) => ()
            case ListLiteral(first, rest) => ()
            case IfExpr(cond, ifThen, ifElse) => ()
            case LetExpr(decls, expr) => ()
            case CaseExpr(expr, nilAlt, consAlt) => ()
            case SelectExpr(sel, tuple) => ()
            case TupleLiteral(first, rest) => ()
            case ApplyExpr(fun, params) => ()
            case LetrecExpr(decls, expr) =>
                processLetrec(decls, env)
                resolveLinks(expr, env) // TODO: better env
            case Unary(_, expr) => ()
            case Binary(_, left, right) => ()
            case Id(name) =>
                // TODO
                ()
            case _ =>
                println("Unprocessed: " + node)
                ()
        }
    }

    def processLetrec(decls: List[Decl], env: Env) {
        val newEnv = env ++ getBindings(decls)
    }

    def getBindings(decls: List[Decl]) = {
        def get(decl: Decl) = decl match {
            case FunDecl(IdLeft(id), _) =>
                List(id)
            case TupleDecl(TupleLeft(h, t), _) =>
                h :: t
        }

        def makePair(id: Id) = (id.text, id)

        decls.flatMap(get).map(makePair).toMap
    }
}