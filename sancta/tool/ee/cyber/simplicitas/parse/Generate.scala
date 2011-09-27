// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import java.io.{OutputStreamWriter, FileOutputStream, File, IOException}
import java.util.IdentityHashMap

import org.antlr.runtime._
import org.antlr.runtime.tree._

import org.apache.tools.ant.BuildException
import scala.collection.mutable.ArrayBuffer


class GrammarParser(baseDir: String, encoding: String) {
    type posMapType = java.util.IdentityHashMap[Any, (String, BaseTree)]

    private var posMap: posMapType = null
    var tree: Object = null

    def parse(inputFile: String) {
        posMap = new posMapType()
        tree = parseImpl(inputFile,
            new ANTLRFileStream(inputFile, encoding))
    }

    def getPos(node: Any): Option[(String, Int, Int)] = {
        val p = posMap.get(node)
        if (p eq null)
            None
        else
            Some((p._1, p._2.getLine, p._2.getCharPositionInLine + 1))
    }

    private def parseImpl(fileName: String, input: CharStream) = {
        val lexer = new AntLikeLexer(input)
        val tokens = new CommonTokenStream(lexer)
        val parser = new AntLikeParser(tokens)
        val parse_result = parser.grammarDef
        if (parser.getNumberOfSyntaxErrors != 0) {
            throw new GrammarException(
                "Syntax errors found in the grammar file")
        }

        val atree = parse_result.getTree.asInstanceOf[CommonTree]
        val converted = convertTree(fileName, atree)
        val withImports = resolveImports(converted)
        println("resolved imports: " + withImports)
        withImports
    }

    private def convertTree(fileName: String, t: BaseTree,
                            a: ArrayBuffer[Object]) {
        def addChildren(t: BaseTree, a: ArrayBuffer[Object]) =
            for (i <- 0 to t.getChildCount - 1) {
                convertTree(fileName, (t getChild i).asInstanceOf[BaseTree], a)
            }

        if (t isNil) {
            addChildren(t, a)
        } else if (t.getChildCount == 0) {
            val id = t.toString
            a += id
            posMap.put(id, (fileName, t))
        } else {
            val items = new ArrayBuffer[Object]
            items += t.toString
            posMap.put(items(0), (fileName, t))
            addChildren(t, items)
            val itemList: List[Any] = items.toList
            a += itemList
            posMap.put(itemList, (fileName, t))
        }
    }

    private def convertTree(fileName: String, t: BaseTree): Object = {
        val result = new ArrayBuffer[Object]()
        convertTree(fileName, t, result)
        result.toList match {
            case List(tree: Object) => tree
            case tree => tree
        }
    }

    private def resolveImports(tree: Object): Object = {
        def readContents(inputFile: String): Object = {
            val fullPath = baseDir + "/" +
                    GrammarUtils.stripQuotes(inputFile)

            parseImpl(fullPath,
                new ANTLRFileStream(fullPath, encoding))
        }

        def tryImport(t: List[Any]): List[Any] = t match {
            case List("import", file: String) :: rest =>
                List("import", readContents(file)) :: tryImport(rest)
            case other =>
                other
        }

        tree match {
            case (packageDef @ ("grammar" :: _)) :: rest =>
                packageDef :: tryImport(rest)
            case _ =>
                tree
        }
    }
}

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

    def generate(inputFile: String, encoding: String) {
        val parser = new GrammarParser(baseName(inputFile), encoding)

        parser.parse(inputFile)

//        println(parser.tree)
        val gen = new GrammarGen(parser.getPos)
        gen.grammargen(parser.tree)

        writeFile(gen.grammarName + ".scala", gen.getScalaSource)
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

    def baseName(file: String) = {
        val p = new java.io.File(file).getParent
        if (p ne null) p else "."
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

/** Ant task for compiling Simpl grammars. */
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
                throw new BuildException(e.getMessage)
        }
    }
}
