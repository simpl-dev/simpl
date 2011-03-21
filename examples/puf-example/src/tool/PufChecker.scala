package ee.cyber.simplicitas.puf

// Resolves all the identifiers. Checks that there are no undefined
// identifiers. Annotates the AST with references.

import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.{CommonNode, SourceMessage}

object PufChecker {
    def process(tree: Program) = {
        val ctx = new PufChecker(tree)
        ctx.process()
        ctx.errors
    }
}

private class PufChecker(tree: Program) {
    /** Environment is mapping from variable names to Id nodes that
     * define these variables. */
    type Env = Map[String, CommonNode]

    /** List of erroes discovered during analysis. */
    val errors = ArrayBuffer[SourceMessage]()

    /** The main entry point. */
    def process() {
        resolveLinks(tree, Map.empty)
    }

    /** The workhorse -- takes as an input a node and environment.
      * Fills target fields of the identifiers. */
    def resolveLinks(node: CommonNode, env: Env) {
        node match {
            case Program(_, decls) =>
                // Program is essentially a letrec
                processLetrecDecls(decls, env)
                if (!getBindings(decls).contains("main")) {
                    addError(null,
                        "The program must declare top-level symbol 'main'")
                }
            case FunExpr(params, expr) =>
                // Add bindings for formal parameters.
                val newEnv = env ++ params.map(makeBinding)
                // Resolve links in body.
                resolveLinks(expr, newEnv)
                // Verify that there are no duplicate parameter names.
                checkUnique(params)
            case ConsExpr(left, right) =>
                resolveLinks(left, env)
                resolveLinks(right, env)
            case ListLiteral(items) =>
                items.foreach(resolveLinks(_, env))
            case IfExpr(cond, ifThen, ifElse) =>
                resolveLinks(cond, env)
                resolveLinks(ifThen, env)
                resolveLinks(ifElse, env)
            case LetrecExpr(decls, expr) =>
                processLetrecDecls(decls, env)
                resolveLinks(expr, env ++ getBindings(decls))
            case LetExpr(decls, expr) =>
                processLet(decls, expr, env)
                checkUnique(decls.flatMap(getIds))
            case CaseExpr(expr, NilAlt(nilAlt), ConsAlt(h, t, consAlt)) =>
                resolveLinks(expr, env)
                resolveLinks(nilAlt, env)
                // When doing consAlt, also add bindings for
                // head and tail variables.
                resolveLinks(
                    consAlt,
                    env ++ List(h, t).map(makeBinding).toMap)
                // head and tail must not be assigned to same variable.
                checkUnique(List(h, t))
            case SelectExpr(sel, tuple) =>
                resolveLinks(tuple, env)
            case TupleLiteral(items) =>
                items.foreach(resolveLinks(_, env))
            case ApplyExpr(fun, params) =>
                (fun :: params).foreach(resolveLinks(_, env))
            case Unary(_, expr) =>
                resolveLinks(expr, env)
            case Binary(_, left, right) =>
                resolveLinks(left, env)
                resolveLinks(right, env)
            case Id(name) =>
                if (env.contains(name)) {
                    // Environment contains definitoin for this variable.
                    // Make this var point to the definition.
                    node.asInstanceOf[Id].target = env(name)
                } else {
                    // Unknown identifier.
                    addError(node, "Unknown identifier: " + name)
                }
            case _ =>
                // This is uninteresting node (number or empty list),
                // no need to analyze it.
                ()
        }
    }

    def processLetrecDecls(decls: List[Decl], env: Env) {
        // The right-hand sides of the declarations can see left-hand sides
        // of the other declarations.
        val newEnv = env ++ getBindings(decls)
        for (decl <- decls) {
            resolveLinks(decl.expr, newEnv)
        }

        checkUnique(decls.flatMap(getIds))
    }

    /** The let construction in PUF is similar to let* in Scheme --
     * variable definition is in scope of all the following definitions. */
    def processLet(decls: List[Decl], expr: CommonNode, env: Env) {
        var newEnv = env

        for (decl <- decls) {
            // Check the right-hand side
            resolveLinks(decl.expr, newEnv)

            // Next definitions can see this variable.
            newEnv ++= getBindings(List(decl))
        }

        // The main expression can see all the variables.
        resolveLinks(expr, newEnv)
    }

    def makeBinding(id: Id) =
        (id.text, id)

    /** Gather all the identifiers from the declarations. */
    def getIds(decl: Decl) = decl match {
        case FunDecl(IdLeft(id), _) =>
            List(id)
        case TupleDecl(TupleLeft(items), _) =>
            items
    }

    /** Get variable bindings from list of declarations. */
    def getBindings(decls: List[Decl]) = {
        decls.flatMap(getIds).map(makeBinding).toMap
    }

    /** Give error if the list contains duplicate identifiers. */
    def checkUnique(idList: List[Id]) {
        val checked = collection.mutable.Set[String]()

        for (id <- idList) {
            if (checked.contains(id.text)) {
                addError(id, "Duplicate variable name: " + id.text)
            }
            checked += id.text
        }
    }

    def addError(node: CommonNode, message: String) {
        errors += new SourceMessage(message, SourceMessage.Error, node)
    }
}