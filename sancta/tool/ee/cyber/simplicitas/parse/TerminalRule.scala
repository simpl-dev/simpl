// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer
import GrammarUtils._

/* This file contains classes for terminal and fragment rules. */

/** Classes for storing operations on lexer states. */
abstract class StateOp(val method: String, val states: List[Int])
case class NoStateOp() extends StateOp(null, null)
case class EnterState(s: List[Int]) extends StateOp("enter", s)
case class ExitState(s: List[Int]) extends StateOp("exit", s)
case class CheckAll(s: List[Int]) extends StateOp("checkAll", s)
case class CheckAny(s: List[Int]) extends StateOp("checkAny", s)

/** Base class for terminal and fragment, but not for literal rules.
 * Such is life. */
abstract class TerminalFragment(pName: String, pTree: List[Any],
                                symbols: SymbolTable)
        extends Rule(pName, pTree, symbols) {
    import symbols._

    /** List of lexer state manipulation commands. */
    var stateOps: Set[StateOp] = Set.empty

    override def antlrName = name.capitalize

    // Checks whether this rule consists of string literal, similar to
    // automatically generated keyword rules. If so, registers the keyword.
    // This code is run from constructor so that the keywords are all available
    // when non-terminal rules are analyzed.
    addKeywords()

    def collectParams() {
        // Terminal rules do not have parameters.
    }

    def isTerminalRule = true

    // The terminal rules do not have parameters. Also, terminal rules also
    // need to match the state operations in the beginning of the rule.
    override def analyze() {
        matchBody()
        matchStateOps()
    }

    /** Processes the lexer state manipulation commands in the beginning
     * of the rule. */
    private def matchStateOps() {
        def loop(node: List[Any]): List[Any] = node match {
            case ("enter-state" :: states) :: rest =>
                if (getEnterOp != None) {
                    error(node.head, "Duplicated enter-state directive")
                }
                stateOps += EnterState(states.map(stateIndex))
                loop(rest)
            case ("exit-state" :: states) :: rest =>
                if (getExitOp != None) {
                    error(node.head, "Duplicated exit-state directive")
                }
                stateOps += ExitState(states.map(stateIndex))
                loop(rest)
            case ("check-all" :: states) :: rest =>
                if (getCheckOp != None) {
                    error(node.head,
                        "Duplicated check-any or check-all directive")
                }
                stateOps += CheckAll(states.map(stateIndex))
                loop(rest)
            case ("check-any" :: states) :: rest =>
                if (getCheckOp != None) {
                    error(node.head,
                        "Duplicated check-any or check-all directive")
                }
                stateOps += CheckAny(states.map(stateIndex))
                loop(rest)
            case _ =>
                node
        }

        tree = loop(tree)
    }

    /** Returns index of state with given name. */
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

        val checkOp = getCheckOp
        if (checkOp != None) {
            buf += "{__lexerState." + checkOp.get.method + "(new int[] {" +
                    checkOp.get.states.mkString(",") + "})}?=> "
        }

        buf += "("
        doOptionList(matchTerminal, tree)
        buf += ")"

        val exitOp = getExitOp
        if (exitOp != None) {
            buf += "{__lexerState.exit(new int[] {" +
                    exitOp.get.states.mkString(",") + "});}"
        }
        val enterOp = getEnterOp
        if (enterOp != None) {
            buf += "{__lexerState.enter(new int[] {" +
                    enterOp.get.states.mkString(",") + "});}"
        }
    }

    // Helper functions to find state operations with given type.

    private def getCheckOp =
        stateOps.find(
            (o: StateOp) => o match {
                case CheckAny(_) | CheckAll(_) => true
                case _ => false
            })

    private def getEnterOp =
        stateOps.find(_.isInstanceOf[EnterState])

    private def getExitOp =
        stateOps.find(_.isInstanceOf[ExitState])

    /** Returns name of the token that is displayed with parser errors. */
    def tokenName = {
        /** If the rule pattern is in the form of "foo" | "bar" | "baz",
          * return list of alternatives. Otherwise return None. */
        def getAlts: Option[List[String]] = {
            // This will contain extracted alternatives.
            val ret = new ArrayBuffer[String]()

            for (alt <- tree) {
                alt match {
                    // It is a single string. Add to list.
                    case List("NODE", List("MATCH", s))
                            if s.isInstanceOf[String] =>
                        ret += s.asInstanceOf[String]
                    // Pattern is more complex than planned. Return "not found".
                    case _ =>
                        return None
                }
            }

            Some(ret.toList)
        }

        /** Join alternatives together using construct "A, B, C or D". */
        def joinAlts(alts: List[String]): String = alts match {
            case single :: Nil =>
                single
            case item :: last :: Nil =>
                item + " or " + last
            case item :: rest =>
                item + ", " + joinAlts(rest)
            case _ =>
                // Tree is supposed to contain at least something.
                throw new IllegalArgumentException(
                    "Invalid tree at joinAlts: " + tree)
        }

        tree match {
            case List(List("NODE", List("MATCH", txt))) =>
                txt
            case _ =>
                // Try to see if the patten has the form: "foo" | "bar"
                getAlts match {
                    case Some(alts) =>
                        joinAlts(alts)
                    case None =>
                        // Nope. Just return the name of rule hoping that it
                        // is meaningful.
                        name
                }
        }
    }

    /** Adds to keyword map all the keywords that are matched by this rule. */
    def addKeywords() {
        tree match {
            case List(List("NODE", List("MATCH", txt: String))) =>
                keywords += (txt -> name)
            // TODO: recognize more versions of string literals.
            case _ =>
                ()
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
class LiteralRule(pName: String, val text: String, symbols: SymbolTable)
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