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
        val isList: Boolean) {
    /** The actual Scala type of the parameter. */
    var paramType: String = null

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
}

class FragmentRule(pName: String, pTree: List[Any], symbols: STable)
        extends Rule(pName, pTree, symbols) {
    def collectParams() {}

    override def generateClasses() = super.generateClasses()
}

class TerminalRule(pName: String, hidden: Boolean, pTree: List[Any],
        symbols: STable) extends Rule(pName, pTree, symbols) {
    import symbols._

    def collectParams() {}

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
        extends Rule(pName, List(List("NODE", stripQuotes(text))), symbols) {
    returnType = "LiteralNode"

    def collectParams() {}
    // Literal rules do not generate any classes.
    override def generateClasses() {}
}

abstract class NonterminalRule(pName: String, pTree: List[Any], symbols: STable)
        extends Rule(pName, pTree, symbols) {
}

class OptionRule(pName: String, pTree: List[Any], symbols: STable)
        extends NonterminalRule(pName, pTree, symbols) {
    import symbols._

    /** Finds and records all the rule parameters. */
    def collectParams() {
        // Nothing to do: tree already contains list of called rules.
    }

    override def generateClasses() {
        classes(name) = new RClass(name, "trait", body)
        for (opt <- tree) {
            actions.addBinding(opt.toString, addExtend(name))

            // If called rule has some weird return type then we
            // must extend this return type, otherwise the rule call
            // will not be type correct.
            val calledRule = rules(opt.toString)
            if (calledRule.returnType ne null) {
                actions.addBinding(actualReturnType,
                        addExtend(calledRule.returnType))
            }
        }

        super.generateClasses()
    }
}

object Modifier extends Enumeration("?", "*", "+") {
    type Val = Value

    val Optional, Star, Plus = Value
}

class NormalRule(pName: String, pTree: List[Any], symbols: STable)
        extends NonterminalRule(pName, pTree, symbols) {
    import symbols._

    /** Identifies current branch in the options. */
    var currentBranch: BranchIdentifier = BranchIdentifier.empty

    /** whether the currently analyzed branch contains
      * repetition (Foo+ or Foo*). */
    var isList = false

    /** Finds and records all the rule parameters. */
    def collectParams() {
        doOptionList(tree)
    }

    def doOptionList(lst: List[Any]) {
        for ("NODE" :: matches <- lst) {
            for (m <- matches) {
                m match {
                    case List("MATCH", modifier: String, ruleCall) =>
                        doMatch(Modifier.withName(modifier), ruleCall)
                    case List("MATCH", ruleCall) =>
                        doMatch(null, ruleCall)
                }
            }
            currentBranch = currentBranch.nextBranch
        }
    }

    def doMatch(modifier: Modifier.Val, ruleCall: Any) {
        val oldList = isList
        if ((modifier eq Modifier.Plus) || (modifier eq Modifier.Star)) {
            isList = true
        }
        ruleCall match {
            // Foo
            case pattern: String =>
                doRuleCall(null, pattern)
            // foo=Bar
            case List("=", name: String, pattern: String) =>
                doRuleCall(name, pattern)
            // (foo)
            case "(" :: options => 
                val oldBranch = currentBranch

                currentBranch = currentBranch.extend
                doOptionList(options)

                currentBranch = oldBranch
        }
        isList = oldList
    }

    def doRuleCall(name: String, pattern: String) {
        println("doRuleCall(" + name + ", " + pattern + "), branch=" +
                currentBranch + ", list=" + isList)

        // Unnamed call to literal "foo"
        if ((name eq null) && pattern.startsWith("'")) {
            addLiteralRule(pattern)
            return
        }

        var calledRuleName = pattern

        // handle calls to named literals: foo="bar"
        if (pattern.startsWith("'")) {
            addLiteralRule(pattern)
            calledRuleName = keywords(pattern)
        }

        val varName = if (name eq null) uncapitalize(pattern) else name

        val param = new RParam(varName, calledRuleName, currentBranch, isList)

        checkParamNameConflicts(param)
    }

    def addLiteralRule(pattern: String) {
        if (keywords.contains(pattern)) {
            return
        }

        // TODO: try to make better variable name.
        val ruleName = newId
        rules(ruleName) = new LiteralRule(ruleName, pattern, symbols)
        keywords(pattern) = ruleName
    }

    // Check if name of the node conflicts with some other name in
    // the same branch. For example:
    // x=foo y=bar x=baz is a conflict.
    // y=bar (x=foo | x=baz) is not because both x's are in different
    // branches.
    def checkParamNameConflicts(np: RParam) {
        println("checkParamNameConflicts(" + np.name + ", " + np.rule + ")")
        val varName = np.name

        params find (_.name == varName) match {
            // This rule already has parameter with this name.
            // Check if there are conflicts.
            case Some(other) =>
                if (currentBranch.conflictsWith(other.branch)) {
                    throw new Exception("multiple tokens named '" + varName + "'")
                } else if (other.rule != np.rule) {
                    // TODO: better check for type conflict.
                    throw new Exception("token type conflict: " + varName +
                          " was " + other.rule + ", but redefined as " +
                          np.rule)
                } else {
                    // The compatible parameter already exists, do nothing.
                    ()
                }
            case _ =>
                params += np
        }
    }

    override def generateClasses() {
        println("generateClasses(" + name + ")")
        val cl = new RClass(name, "case class", body)
        classes(name) = cl

        for (p <- params) {
            println("param: " + p)
            if (!rules.contains(p.rule)) {
                throw new Exception("Invalid rule reference: " + name + "." +
                        p.name)
            }
            val ruleType = rules(p.rule).actualReturnType
            cl.params += new RCParam(p.name,
                    if (p.isList) "List[" + ruleType + "]" else ruleType)
        }

        super.generateClasses()
    }
}
