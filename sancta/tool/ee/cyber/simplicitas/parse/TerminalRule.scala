package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._

abstract class TerminalFragment(pName: String, pTree: List[Any], symbols: STable)
        extends Rule(pName, pTree, symbols) {
    override def antlrName = name.capitalize
    def collectParams() {}
    def isTerminalRule = true

    override def ruleBody(implicit buf: ArrayBuffer[String]) {
        println("generating " + name + ": " + tree)
        doOptionList(matchTerminal, tree)
    }

    def matchTerminal(node: Any)(implicit buf: ArrayBuffer[String]) {
        println("node = |" + node + "|")
        node match {
            // ( Foo )
            case "(" :: alt =>
                buf += "("
                doOptionList(matchTerminal, alt)
                buf += ")"
            // Foo .. Bar
            case ".." :: (from: String) :: (to: String) :: Nil =>
                buf += " " + from + ".." + to
            // Just identifier or string.
            // TODO: check if rule references are correct. Also,
            // capitalize the rule names.
            case s: String =>
                buf += s
            // To suppress a warning
            case _ =>
                throw new IllegalArgumentException(node.toString)
        }
    }

    def paramValue(param: RParam) = {
        val varName = "$" + param.antlrName
        val v = "(" + name + ")setTokenPos(new " + name +
                "(" + varName + ".getText()" + ")," + varName + ")"

        varName + "==null?null:" + v
    }
}


class FragmentRule(pName: String, pTree: List[Any], symbols: STable)
        extends TerminalFragment(pName, pTree, symbols) {
    override def generateClasses() = super.generateClasses()
}

class TerminalRule(pName: String, hidden: Boolean, pTree: List[Any],
        symbols: STable) extends TerminalFragment(pName, pTree, symbols) {
    import symbols._

    override def generateClasses() {
        // Hidden rules do not contribute to the AST and therefore
        // should generate no classes.
        if (!hidden) {
            val newClass = new RClass(name, "case class", body)
            newClass.extend += "TerminalNode"
            newClass.params += new RCParam("text", "String")
            classes(name) = newClass
        }

        super.generateClasses()
    }
}

class LiteralRule(pName: String, text: String, symbols: STable)
        extends Rule(
                pName, 
                List(List("NODE", List("MATCH", stripQuotes(text)))),
                symbols) {
    returnType = "LiteralNode"

    override def antlrName = name.capitalize
    def collectParams() {}
    // Literal rules do not generate any classes.
    override def generateClasses() {}
    def isTerminalRule = true

    override def ruleBody(implicit buf: ArrayBuffer[String]) {
        buf += text
    }

    def paramValue(param: RParam) = {
        val varName = "$" + param.antlrName
        val v = "(" + name + ")setTokenPos(new " + name +
                "(" + varName + ".getText()" + ")," + varName + ")"

        varName + "==null?null:" + v
    }
}