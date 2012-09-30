package ee.cyber.simplicitas.exprewriting

import ee.cyber.simplicitas.MainBase
;

object ExpMain extends MainBase {
    def main(argv: Array[String]) {
        val grammar = new ExpGrammar()

        for (val s <- argv) {
            println("\nProcessing: " + s)
            grammar.parseString(s)
            checkErrors(grammar.errors)
            Simplify.simplify(grammar.tree)
        }
    }
}
