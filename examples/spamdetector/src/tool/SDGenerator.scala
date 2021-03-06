package ee.cyber.simplicitas.spamdetector

import ee.cyber.simplicitas.{GeneratorBase, MainBase}
import java.io.FileWriter

class SDGenerator(destDir: String)
        extends GeneratorBase(destDir) {
    val templates = getTemplates("SD.stg")

    def generate(program: Program) {
        val args = program.toJavaMap("nType")
        writeFile("GeneratedProgram.java",
                templates.getInstanceOf("program", args))
    }
}

object SDMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new SDGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            val resolver = new ResolverScala
            resolver.resolveReferences(grammar.tree)
            checkErrors(resolver.errors)

            new SDGenerator(destDir).generate(grammar.tree)
            generatePretty(grammar.tree)
        }
    }

    def generatePretty(program: Program) {
        val writer = new FileWriter(destDir + "/GeneratedPretty.java")
        CodegenPP.prettyPrint(program, writer)
        writer.close()
    }
}
