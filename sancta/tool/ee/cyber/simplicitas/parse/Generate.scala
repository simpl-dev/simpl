// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import java.io.{OutputStreamWriter, FileOutputStream, File, IOException}
import java.util.IdentityHashMap

import org.antlr.runtime._
import org.antlr.runtime.tree._

import org.apache.tools.ant.BuildException
import scala.collection.mutable.ArrayBuffer


class Generator {
    var outputDir = "."
    var runANTLR = true

    def writeFile(file: String, contents: String) {
        val writer = new OutputStreamWriter(
            new FileOutputStream(outputDir + "/" + file),
            "UTF-8")
        writer.write(contents)
        writer.close()
    }

    type posmap = java.util.IdentityHashMap[Any, BaseTree]

    def convertTree(t: BaseTree, a: ArrayBuffer[Object], pm: posmap) {
        def addChildren(t: BaseTree, a: ArrayBuffer[Object]) =
            for (i <- 0 to t.getChildCount - 1)
                convertTree((t getChild i).asInstanceOf[BaseTree], a, pm)

        if (t isNil) {
            addChildren(t, a)
        } else if (t.getChildCount == 0) {
            val id = t.toString
            a += id
            pm.put(id, t)
        } else {
            val items = new ArrayBuffer[Object]
            items += t.toString
            pm.put(items(0), t)
            addChildren(t, items)
            val itemList: List[Any] = items.toList
            a += itemList
            pm.put(itemList, t)
        }
    }

    def convertTree(t: BaseTree, pm: posmap): Object = {
        val result = new ArrayBuffer[Object]()
        convertTree(t, result, pm)
        (result toList) match {
            case List(tree: Object) => tree
            case tree => tree
        }
    }

    def generate(inputFile: String, encoding: String) {
        generate(new ANTLRFileStream(inputFile, encoding))
    }

    def generate(input: java.io.Reader) {
        generate(new ANTLRReaderStream(input))
    }

    def generate(input: CharStream) {
        val lexer = new AntLikeLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new AntLikeParser(tokens)
        val parse_result = parser.grammarDef
        if (parser.getNumberOfSyntaxErrors != 0) {
            throw new GrammarException(
                "Syntax errors found in the grammar file")
        }
        val atree = parse_result.getTree.asInstanceOf[CommonTree]
        val pm = new posmap()
        val gtree = convertTree(atree, pm)
        def getPos(node: Any): List[Int] = {
            val p = pm.get(node)
            if (p == null) Nil else List(p.getLine, p.getCharPositionInLine + 1)
        }

        val gen = new Gen2(getPos)
        gen.grammargen(gtree)
        writeFile(gen.grammarName + ".scala", gen.getTreeSource)
        val grammarFile = gen.grammarName + ".g"
        writeFile(grammarFile, gen.getGrammarSource)

        if (runANTLR) {
            val tool = new org.antlr.Tool(Array(
                "-o", outputDir, "-lib", outputDir,
                outputDir + "/" + grammarFile))
            tool.process
            if (org.antlr.tool.ErrorManager.getNumErrors > 0)
                throw new GrammarException("ANTLR failed")
        }
    }
}

object Generate {

    def main(argv: Array[String]) {
        if (argv.length != 2 && argv.length != 4)
            usage()

        val gen = new Generator()
        var inputFile: String  = null
        for (i <- 0 to argv.length - 1 by 2) {
            argv(i) match {
                case "-in" =>
                    inputFile = argv(i + 1)
                case "-dir" =>
                    gen.outputDir = argv(i + 1)
            }
        }

        if (inputFile eq null)
            usage()

        try {
            gen.generate(inputFile, "UTF-8")
        } catch {
            case e: GrammarException =>
                println(e getMessage)
                System exit 1
        }
    }

    def usage() {
        Console.err printf
            "Usage:\nGenerate -in <grammar file> [-dir <output directory>]\n"
        exit(1)
    }
}

class GrammarTask extends org.apache.tools.ant.Task {
    private var destDir = ""
    private var src = ""

    def setSrc(source: String) {
        src = source
    }

    def setDestDir(target: String) {
        destDir = target
    }

    override def execute() {
        if (src == "")
            throw new BuildException("src attribute is missing")
        if (destDir == "")
            throw new BuildException("destDir attribute is missing")
        log("Generating parser from " + src + " to " + destDir)
        try {
            new File(destDir) mkdirs
        } catch {
            case e: IOException =>
                throw new BuildException(destDir + ": " + e.getMessage)
        }
        val gen = new Generator()
        gen.outputDir = destDir
        try {
            gen.generate(src, "UTF-8")
        } catch {
            case e: GrammarException =>
                throw new BuildException(src + ":" + e.getMessage)
        }
    }
}
