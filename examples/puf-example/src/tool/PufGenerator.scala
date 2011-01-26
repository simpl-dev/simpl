package ee.cyber.simplicitas.puf;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import ee.cyber.simplicitas.PrettyPrint._

class PufGenerator(destDir: String)
        extends GeneratorBase(destDir) {
    def generate(tree: Program) {
        println(prettyPrint(tree))
    }
}

object PufMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new PufGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            val checkerErrors = PufChecker.process(grammar.tree)
            checkErrors(checkerErrors)

            new PufGenerator(destDir).generate(grammar.tree)
        }
    }
}
