package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._

/** Rule parameter.
  * @param name The name of the parameter.
  * @param rule Name of the rule called.
  * @param branch Identifies the branch
  * @param isList Whether the parameter is used in + or * context.
  */
class RParam(val name: String, val rule: String, val branch: BranchIdentifier,
        val isList: Boolean, symbols: STable) {
    /** The actual Scala type of the parameter. */
    var paramType: String = null

    /** The name of the parameter in the ANTLR grammar rule. */
    var antlrName: String = symbols.newId

    override def toString = name + ": " + 
        (if (paramType eq null) rule else paramType + "(" + rule + ")") +
        (if (isList) ", LIST " else " ") + branch
}

object Actions {
    type Action = (RClass) => Unit
    class ActionSet extends
        collection.mutable.HashMap[String, collection.mutable.Set[Action]]
           with collection.mutable.MultiMap[String, Action]

    def addExtend(cl: String): (RClass) => Unit =
        (rule: RClass) => rule.extend += cl
}

import Actions._

abstract class Rule(val name: String, var tree: List[Any], symbols: STable) {
    var returnType: String = null
    var returnCode: String = null
    var body: String = null

    var params = new ArrayBuffer[RParam]

    val error = GrammarUtils.error(symbols.getPos)_

    import symbols._

    def actualReturnType = if (returnType ne null) returnType else name

    override def toString = name + " returns " + 
            (if (returnType eq null) name else returnType) + " {" + tree +
            "}\nParameters:\n" + params.map(_.toString).mkString("\n")

    def analyze() {
        println("analyze(" + name + ")")
        matchReturns()
        matchBody()
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
                println("matched returns!")
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
                classes(returnType) = new RClass(returnType, "trait", body)
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
        buf += antlrName + ruleReturns
        ruleInit
        ruleAfter
        buf += ":\n    "
        ruleBody
        buf += ";\n"
    }

    def antlrName = name
    def ruleReturns = ""

    def ruleInit(implicit buf: ArrayBuffer[String]) {}
    def ruleAfter(implicit buf: ArrayBuffer[String]) {}
    def ruleBody(implicit buf: ArrayBuffer[String]) {}

    // TODO: some decent implementation.
    def paramValue(param: RParam) = param.antlrName
}
