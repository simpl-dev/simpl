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
                processLetrecDecls(decls, env)
            case FunExpr(params, expr) =>
                val newEnv = env ++ params.map(makeBinding)
                resolveLinks(expr, newEnv)
            case ConsExpr(left, right) =>
                resolveLinks(left, env)
                resolveLinks(right, env)
            case ListLiteral(first, rest) =>
                (first :: rest).foreach(resolveLinks(_, env))
            case IfExpr(cond, ifThen, ifElse) =>
                resolveLinks(cond, env)
                resolveLinks(ifThen, env)
                resolveLinks(ifElse, env)
            case LetExpr(decls, expr) =>
                processLet(decls, expr, env)
            case CaseExpr(expr, NilAlt(nilAlt), ConsAlt(h, t, consAlt)) =>
                resolveLinks(expr, env)
                resolveLinks(nilAlt, env)
                resolveLinks(
                    consAlt,
                    env ++ List(h, t).map(makeBinding).toMap)
            case SelectExpr(sel, tuple) =>
                resolveLinks(tuple, env)
            case TupleLiteral(first, rest) =>
                (first :: rest).foreach(resolveLinks(_, env))
            case ApplyExpr(fun, params) =>
                (fun :: params).foreach(resolveLinks(_, env))
            case LetrecExpr(decls, expr) =>
                processLetrecDecls(decls, env)
                resolveLinks(expr, env ++ getBindings(decls))
            case Unary(_, expr) =>
                resolveLinks(expr, env)
            case Binary(_, left, right) =>
                resolveLinks(left, env)
                resolveLinks(right, env)
            case Id(name) =>
                if (env.contains(name)) {
                    node.asInstanceOf[Id].target = env(name)
                } else {
                    errors += new SourceMessage(
                        "Unknown identifier: " + name,
                        SourceMessage.Error,
                        node)
                }
            case _ =>
                println("Unprocessed: " + node)
                ()
        }
    }

    def processLetrecDecls(decls: List[Decl], env: Env) {
        val newEnv = env ++ getBindings(decls)
        for (decl <- decls) {
            resolveLinks(decl.expr, newEnv)
        }
    }

    def processLet(decls: List[Decl], expr: CommonNode, env: Env) {
        var newEnv = env

        for (decl <- decls) {
            resolveLinks(decl.expr, newEnv)
            newEnv ++= getBindings(List(decl))
        }

        resolveLinks(expr, newEnv)
    }

    def makeBinding(id: Id) =
        (id.text, id)

    def getBindings(decls: List[Decl]) = {
        def get(decl: Decl) = decl match {
            case FunDecl(IdLeft(id), _) =>
                List(id)
            case TupleDecl(TupleLeft(h, t), _) =>
                h :: t
        }


        decls.flatMap(get).map(makeBinding).toMap
    }
}