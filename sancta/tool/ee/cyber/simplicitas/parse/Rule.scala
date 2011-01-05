package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import Actions._

/** Information about other rule that is called from current rule.
  * @param name variable name for rule in the grammar ("foo" in foo=Bar).
  * @param rule Name of the rule called.
  * @param branch Identifies the branch.
  * @see BranchIdentifier class for further details.
  * @param isList Whether the parameter is used in + or * context.
  */
class RuleParam(val name: String, val rule: String,
        val branch: BranchIdentifier, val isList: Boolean,
        symbols: SymbolTable) {
    /** The actual Scala type of the parameter. */
    var paramType: String = null

    /** The name of the parameter in the ANTLR grammar rule. */
    var antlrName: String = symbols.newId

    /** Name of the temporary variable used to collect
      * the list elements (used if this parameter is a list). */
    val listVar: String = if (isList) symbols.newId else null

    override def toString = name + ": " +
        (if (paramType eq null) rule else paramType + "(" + rule + ")") +
        (if (isList) ", LIST " else " ") + branch
}

/** Represents a rule in the grammar. Also responsible for generating the
  * corresponding Scala class(es) and the ANTLR rule.
  * The main work is done in subclasses of Rule. */
abstract class Rule(val name: String, var tree: List[Any], symbols: SymbolTable) {
    var returnType: String = null
    var returnCode: String = null
    var body: String = null

    var params = new ArrayBuffer[RuleParam]

    val error = GrammarUtils.error(symbols.getPos)_

    import symbols._

    def actualReturnType = if (returnType ne null) returnType else name

    override def toString = name + " returns " +
            (if (returnType eq null) name else returnType) + " {" + tree +
            "}\nParameters:\n" + params.map(_.toString).mkString("\n")

    def analyze() {
        matchBody()
        matchReturns()
        collectParams()
    }

    /** Analyzes the "returns foo {bar}" part. */
    def matchReturns() {
        def matchReturnArg(arg: Any) {
            arg match {
                case rt: String =>
                    returnType = rt
                case List("BODY", body) =>
                    returnCode = body.toString
                case _ =>
                    println("Invalid return arg: " + arg)
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

    def matchBody() {
        tree match {
            case List("BODY", b: String) :: rest =>
                body = b
                tree = rest
            case _ =>
                ()
        }
    }

    def collectParams(): Unit

    def generateClasses() {
        // Foo returns Bar => case class Foo extends Bar
        if (returnType ne null)
            actions.addBinding(name, addExtend(returnType))

        // Rule: if rule returns type that does not match any rule,
        // then create new trait and make rule extend this trait.
        if (returnType ne null) {
            if (!rules.contains(returnType) &&
                    !classes.contains(returnType)) {
                classes(returnType) = new RuleClass(returnType, "trait", body)
            }
            classes(name).extend += returnType
        }
    }

    def generateGrammar(buf: StringBuilder) {
        val arrayBuf = new ArrayBuffer[String]
        generateGrammar(arrayBuf)
        for (s <- arrayBuf) {
            buf.append(s)
        }
    }

    def generateGrammar(implicit buf: ArrayBuffer[String]) {
        buf += rulePrefix + antlrName + ruleReturns
        ruleInit
        ruleAfter
        buf += ":\n    "
        ruleBody
        buf += ";\n"
    }

    def antlrName = name
    def ruleReturns = ""

    def rulePrefix = ""
    def ruleInit(implicit buf: ArrayBuffer[String]) {}
    def ruleAfter(implicit buf: ArrayBuffer[String]) {}
    def ruleBody(implicit buf: ArrayBuffer[String]) {}

    // TODO: some decent implementation.
    def paramValue(param: RuleParam): String

    def isTerminalRule: Boolean
}
