package ee.cyber.simplicitas.grammartest

import java.io.FileWriter

import ee.cyber.simplicitas.GeneratorBase
import ee.cyber.simplicitas.grammartest.simpl.SimplGrammar

import Dump._

object Main {
    def main(args: Array[String]) {
        val testDir = args(0)

        val grammar = new SimplGrammar()
        grammar.parseFile(testDir + "/simpl/test1.in")
        val dumped = dumpNode(grammar.tree)
        println(dumped)
        
        writeFile(testDir + "/simpl/test1.out", dumped)
        // TODO: compare dumped node with given input.
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