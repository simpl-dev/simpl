package ee.cyber.simplicitas.grammartest

import java.io.File
import ee.cyber.simplicitas.GenericGrammar
import java.io.FileWriter

import ee.cyber.simplicitas.GeneratorBase
import ee.cyber.simplicitas.grammartest.simpl.SimplGrammar

import Dump._

object Main {
    def main(args: Array[String]) {
        val testDir = args(0)

        val grammars = new File(testDir).listFiles
        for (grDir <- grammars) {
            runGrammar(grDir.getName, grDir)
        }
    }

    def runGrammar(name: String, dir: File) {
        val className = "ee.cyber.simplicitas.grammartest." + name +
                "." + name.capitalize + "Grammar"
        val constructor = Class.forName(className).getConstructor()

        for (testFile <- dir.listFiles if testFile.getName.endsWith(".in")) {
            println("Running grammar " + name + " for file " + testFile.getName)

            val grammar = constructor.newInstance()
                    .asInstanceOf[GenericGrammar]
            grammar.parseFile(testFile.getAbsolutePath)
            val dumped = dumpNode(grammar.tree)
            println(dumped)

            // TODO: compare dumped node with given input.
            writeFile(
                    testFile.getAbsolutePath.replaceAll(".in", ".out"),
                    dumped)
        }
    }

    def writeFile(file: String, contents: String) {
        println("Writing to file " + file)
        val writer = new FileWriter(file)
        try {
          writer.write(contents)
        } finally {
          writer.close()
        }
    }
}