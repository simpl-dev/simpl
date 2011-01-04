package ee.cyber.simplicitas.grammartest

import java.io.FileInputStream
import java.io.BufferedInputStream
import org.junit.Assert
import java.io.File
import java.io.FileWriter

import ee.cyber.simplicitas.GenericGrammar

import Dump._

object Main extends Assert {
    var inputDir: String = null
    var outputDir: String = null

    def main(args: Array[String]) {
        inputDir  = args(0)
        outputDir  = args(1)

        val grammars = new File(inputDir).listFiles
        for (grDir <- grammars) {
            runGrammar(grDir.getName, grDir)
        }
    }

    def runGrammar(name: String, dir: File) {
        val className = "ee.cyber.simplicitas.grammartest." + name +
                "." + name.capitalize + "Grammar"
        val constructor = Class.forName(className).getConstructor()

        new File(outputDir + "/" + name).mkdirs()

        for (testFile <- dir.listFiles if testFile.getName.endsWith(".in")) {
            println("Running grammar " + name + " for file " + testFile.getName)

            val grammar = constructor.newInstance()
                    .asInstanceOf[GenericGrammar]
            grammar.parseFile(testFile.getAbsolutePath)
            if (!grammar.errors.isEmpty) {
                grammar.errors.foreach(println)
//                Assert.fail("Parse errors found")
            }
            val dumped = dumpNode(grammar.tree)

            // Write file to target dir so that it can later be examined
            // if things go wrong.
            val outFile = testFile.getName.replaceAll(".in", ".out")
            writeFile(outputDir + "/" + name + "/" + outFile, dumped)

            Assert.assertEquals(dumped,
                    readFile(inputDir + "/" + name + "/" + outFile))
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

    def readFile(filePath: String) = {
        val buffer = new Array[Byte](new File(filePath).length().intValue)
        var f: BufferedInputStream = null
        try {
            f = new BufferedInputStream(new FileInputStream(filePath))
            f.read(buffer)
        } finally {
            if (f ne null) {
                f.close()
            }
        }
        new String(buffer);
    }
}