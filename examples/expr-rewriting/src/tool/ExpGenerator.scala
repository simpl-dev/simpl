package ee.cyber.simplicitas.exprewriting

import ee.cyber.simplicitas.{MainBase, GeneratorBase}
;

object ExpMain extends MainBase {
    def main(argv: Array[String]) {
        val exprString = argv.mkString(" ")

        val grammar = new ExpGrammar()
        grammar.parseString(exprString)
        checkErrors(grammar.errors)
        simplifyExpression(grammar.tree)
    }

    def simplifyExpression(expr: Expr) {
        println(expr)
    }
}
