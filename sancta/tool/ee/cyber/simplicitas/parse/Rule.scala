// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import Actions._

/** Information about other rule that is called from current rule.
 *
 * @param name variable name for rule in the grammar ("foo" in foo=Bar).
 * @param rule Name of the rule called.
 * @param branch Identifies the branch.
 * @see BranchIdentifier class for further details.
 * @param isList Whether the parameter is used in + or * context.
 */
class RuleParam(val name: String, val rule: String,
        val branch: BranchIdentifier, var isList: Boolean,
        symbols: SymbolTable) {
    /** The actual Scala type of the parameter. */
    var paramType: String = null

    /** The name of the parameter in the ANTLR grammar rule. */
    var antlrName: String = symbols.newId

    /** Name of the temporary variable used to collect
      * the list elements (used if this parameter is a list). */
    var listVar: String = null

    if (isList) {
        makeList()
    }

    def makeList() {
        isList = true
        if (listVar eq null) {
            listVar = symbols.newId
        }
    }

    override def toString = name + ": " +
        (if (paramType eq null) rule else paramType + "(" + rule + ")") +
        (if (isList) ", LIST " else " ") + branch
}

/**
 * Represents a rule in the grammar. Also responsible for generating the
 * corresponding Scala class(es) and the ANTLR rule.
 * The main work is done in subclasses of Rule.
 *
 * @param name Name of the rule.
 */
abstract class Rule(val name: String, var tree: List[Any],
                    symbols: SymbolTable) {
    /** Return type of the rule, if explicitly specified. */
    var returnType: String = null

    /** Return expression for the rule. */
    var returnCode: String = null

    /** Code that will be added to the body of the generated Scala class. */
    var body: String = null

    /** Other rules called by this rule. */
    var params = new ArrayBuffer[RuleParam]

    /** Error reporting function. */
    val error = GrammarUtils.error(symbols.getPos)_

    import symbols._

    /** Returns return type of the method corresponding to this rule.
      * Uses name of the rule if explicit return type is not given. */
    def actualReturnType = if (returnType ne null) returnType else name

    override def toString = name + " returns " +
            (if (returnType eq null) name else returnType) + " {" + tree +
            "}\nParameters:\n" + params.map(_.toString).mkString("\n")

    /**
     * Performs initial analysis of the AST. Basically, processes the
     * tree to extract Scala code, explicit return type and calls to other
     * rules (that will become constructor parameters for generated Scala
     * classes later). */
    def analyze() {
        matchBody()
        matchReturns()
        collectParams()
    }

    /** Analyzes the "returns foo {bar}" part of the rule definition.
     * Modifies the tree so that it now points to the AST after the returns
     * statement (i.e., the actual rule contents). */
    private def matchReturns() {
        def matchReturnArg(arg: Any) {
            arg match {
                case rt: String =>
                    returnType = rt
                case List("BODY", returnBody) =>
                    returnCode = returnBody.toString
                case _ =>
                    ()
            }
        }

        tree match {
            case ("returns" :: returnArgs) :: rest =>
                returnArgs.foreach(matchReturnArg)
                tree = rest
            case _ =>
                ()
        }
    }

    /** Checks for code that will be inserted to generated Scala class. */
    protected def matchBody() {
        tree match {
            case List("BODY", b: String) :: rest =>
                body = b
                tree = rest
            case _ =>
                ()
        }
    }

    /** Analyse the rule body to find all the calls to other rules.
     * The concrete implementation depends on the type of rule. */
    protected def collectParams()

    /** Generate RuleClass objects and insert them to the symbol table.
     * This includes also modifications to existing classes (e.g., adding
     * extends clause).
     *
     * New classes can (and should be) directly generated. However,
     * modifications to other classes should be delayed because the other
     * classes might not be generated yet. Therefore, these modifications
     * should be saved as actions @see SymbolTable.actions
     *
     * @note If child classes override this method (as they should), they
     * must still call this method so that some default actions will be taken.
     */
    def generateClasses() {
        // Foo returns Bar => case class Foo extends Bar
        if (returnType ne null)
            actions.addBinding(name, addExtend(returnType))

        // Rule: if rule returns type that does not match any rule,
        // then create new trait and make rule extend this trait.
        if (returnType ne null) {
            if (!rules.contains(returnType) &&
                    !classes.contains(returnType)) {
                classes(returnType) = new RuleClass(returnType, "trait", null)
            }
            classes(name).extend += returnType
        }
    }

    /** Generates code for ANTLR grammar.
     * @param buf Buffer that will contain the code. */
    def generateGrammar(buf: StringBuilder) {
        val arrayBuf = new ArrayBuffer[String]
        generateGrammar(arrayBuf)
        for (s <- arrayBuf) {
            buf.append(s)
        }
    }

    private def generateGrammar(implicit buf: ArrayBuffer[String]) {
        buf += rulePrefix + antlrName + ruleReturns
        ruleInit
        ruleAfter
        buf += ":\n    "
        ruleBody
        buf += ";\n"
    }

    /** Returns name of the rule as it will appear in the ANTLR code. */
    def antlrName = name

    /** Returns the "returns [Foo]" clause in the ANTLR grammar,
     * if appropriate. */
    protected def ruleReturns = ""

    /** Returns prefix for the ANTLR rule. Used in fragment rules. */
    protected def rulePrefix = ""

    /** Generate the "@init" clause in the ANTLR rule, if necessary. */
    protected def ruleInit(implicit buf: ArrayBuffer[String]) {}

    /** Generate the "@after" clause in the ANTLR rule, if necessary. */
    protected def ruleAfter(implicit buf: ArrayBuffer[String]) {}

    /** Generate the main body of the ANTLR rule. */
    protected def ruleBody(implicit buf: ArrayBuffer[String])

    /** Returns the ANTLR code for constructing value for a parameter.
     * The parameter's name can be read from <code>param</code>.
     * The parameter calls this rule. */
    def paramValue(param: RuleParam): String

    /** Returns true, if this is terminal or fragment rule. */
    def isTerminalRule: Boolean
}
