package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer
import GrammarUtils._

/* This file contains classes for terminal and fragment rules. */

/** Base class for terminal and fragment, but not for literal rules.
 * Such is life. */
abstract class TerminalFragment(pName: String, pTree: List[Any], symbols: SymbolTable)
        extends Rule(pName, pTree, symbols) {
    override def antlrName = name.capitalize

    def collectParams() {
        // Terminal rules do not have parameters.
    }

    def isTerminalRule = true

    /** Helper function for use with doOptionList.
     * Generates grammar for terminal patterns.
     */
    protected def matchTerminal(node: Any)(implicit buf: ArrayBuffer[String]) {
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

    def paramValue(param: RuleParam) = {
        val varName = "$" + param.antlrName
        val v = "(" + name + ")setTokenPos(new " + name +
                "(" + varName + ".getText()" + ")," + varName + ")"

        varName + "==null?null:" + v
    }
}

/** Fragment rules. */
class FragmentRule(pName: String, pTree: List[Any], symbols: SymbolTable)
        extends TerminalFragment(pName, pTree, symbols) {
    override def generateClasses() = super.generateClasses()
    override def rulePrefix = "fragment "

    def ruleBody(implicit buf: ArrayBuffer[String]) {
        doOptionList(matchTerminal, tree)
    }
}

/** Normal terminal rules in the grammar. */
class TerminalRule(pName: String, hidden: Boolean, pTree: List[Any],
        symbols: SymbolTable) extends TerminalFragment(pName, pTree, symbols) {
    import symbols._

    override def generateClasses() {
        // Hidden rules do not contribute to the AST and therefore
        // should generate no classes.
        if (!hidden) {
            val newClass = new RuleClass(name, "case class", body)
            newClass.extend += "TerminalNode"
            newClass.params += new RuleClassParam("text", "String")
            classes(name) = newClass
        }

        super.generateClasses()
    }

    def ruleBody(implicit buf: ArrayBuffer[String]) {
        doOptionList(matchTerminal, tree)
        if (hidden) {
            buf += "{$channel = HIDDEN;}"
        }
    }
}

/** Literal rules are automatically generated and correspond to keywords
 * or operators in the grammar (essentially, everything between
 * quotation marks). They produce rules in the style of
 *
 * Foo: "foo";
 *
 * The literal rules do not generate separate AST classes like normal terminal
 * rules. Instead, all the literal rules will create LiteralNode objects.
 *
 * @param text The literal string that will be matched by this rule.
 */
class LiteralRule(pName: String, text: String, symbols: SymbolTable)
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

    def ruleBody(implicit buf: ArrayBuffer[String]) {
        buf += text
    }

    def paramValue(param: RuleParam) = {
        val varName = "$" + param.antlrName
        val v = "(LiteralNode)setTokenPos(new LiteralNode(" +
                varName + ".getText()" + ")," + varName + ")"

        varName + "==null?null:" + v
    }
}