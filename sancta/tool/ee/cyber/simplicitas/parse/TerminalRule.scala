package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer
import GrammarUtils._

/* This file contains classes for terminal and fragment rules. */

trait StateOp;
case class NoStateOp() extends StateOp
case class EnterState(state: Int) extends StateOp
case class ExitState(states: List[Int]) extends StateOp
case class CheckState(states: List[Int]) extends StateOp

/** Base class for terminal and fragment, but not for literal rules.
 * Such is life. */
abstract class TerminalFragment(pName: String, pTree: List[Any], symbols: SymbolTable)
        extends Rule(pName, pTree, symbols) {
    import symbols._

    var stateOp: StateOp = NoStateOp()

    override def antlrName = name.capitalize

    def collectParams() {
        // Terminal rules do not have parameters.
    }

    def isTerminalRule = true

    override def analyze() {
        matchBody()
        matchStateOps()
    }


    def matchStateOps() {
        tree match {
            case List("enter-state", state) :: rest =>
                stateOp = EnterState(stateIndex(state))
                tree = rest
            case ("exit-state" :: states) :: rest =>
                stateOp = ExitState(states.map(stateIndex))
                tree = rest
            case ("check-state" :: states) :: rest =>
                stateOp = CheckState(states.map(stateIndex))
                tree = rest
            case _ =>
                ()
        }
    }

    private def stateIndex(st: Any) = {
        val idx = lexerStates.indexOf(st.toString)
        if (idx < 0) {
            error(st, "Uknown lexer state: " + st.toString)
        }

        // Won't be returned if error is thrown.
        idx
    }

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

    def ruleBody(implicit buf: ArrayBuffer[String]) {
        // TODO: do not generate these arrays each time, instead use
        // predefined static variables.

        stateOp match {
            case NoStateOp() =>
                doOptionList(matchTerminal, tree)
            case EnterState(state) =>
                buf += "("
                doOptionList(matchTerminal, tree)
                buf += ") "
                buf += "{__lexerState.enter(" + state + ");}"
            case ExitState(states) =>
                buf += "("
                doOptionList(matchTerminal, tree)
                buf += ")"
                buf += "{__lexerState.exit(new int[] {" +
                        states.mkString(",") + "})}"
            case CheckState(states) =>
                buf += "{__lexerState.check(new int[] {" +
                        states.mkString(",") + "})}?=> "
                buf += "("
                doOptionList(matchTerminal, tree)
                buf += ")"
        }
    }
}

/** Fragment rules. */
class FragmentRule(pName: String, pTree: List[Any], symbols: SymbolTable)
        extends TerminalFragment(pName, pTree, symbols) {
    override def generateClasses() = super.generateClasses()
    override def rulePrefix = "fragment "
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

    override def ruleBody(implicit buf: ArrayBuffer[String]) {
        super.ruleBody

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