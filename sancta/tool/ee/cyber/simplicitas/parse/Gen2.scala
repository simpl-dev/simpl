package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._

object Actions {
    type Action = (RClass) => Unit
    class ActionSet extends
        collection.mutable.HashMap[String, collection.mutable.Set[Action]]
           with collection.mutable.MultiMap[String, Action]

    def addExtend(cl: String): (RClass) => Unit =
        (rule: RClass) => rule.extend += cl
}

import Actions._

abstract class Rule(val name: String, var tree: List[Any]) {
    var returnType: String = null
    var returnCode: String = null
    var body: String = null

    var params = new ArrayBuffer[RParam]

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

    def generateClasses(rules: collection.mutable.Map[String, Rule],
            classes: collection.mutable.Map[String, RClass],
            actions: ActionSet): Unit
}

class FragmentRule(pName: String, pTree: List[Any])
        extends Rule(pName, pTree) {
    def collectParams() {}

    def generateClasses(rules: collection.mutable.Map[String, Rule],
            classes: collection.mutable.Map[String, RClass],
            actions: ActionSet) = ()
}

class TerminalRule(pName: String, hidden: Boolean, pTree: List[Any])
        extends Rule(pName, pTree) {
    def collectParams() {}
    def generateClasses(rules: collection.mutable.Map[String, Rule],
            classes: collection.mutable.Map[String, RClass],
            actions: ActionSet) {
        // TODO: make the class extend from TerminalNode
        classes(name) = new RClass(name, "case class")
    }
}

abstract class NonterminalRule(pName: String, pTree: List[Any])
        extends Rule(pName, pTree) {
}

class OptionRule(pName: String, pTree: List[Any])
        extends NonterminalRule(pName, pTree) {
    /** Finds and records all the rule parameters. */
    def collectParams() {
        // Nothing to do: tree already contains list of called rules.
    }

    def generateClasses(rules: collection.mutable.Map[String, Rule],
            classes: collection.mutable.Map[String, RClass],
            actions: ActionSet) {
        classes(name) = new RClass(name, "trait")
        for (opt <- tree) {
            actions.addBinding(opt.toString, addExtend(name))

            // If called rule has some weird return type then we
            // must extend this return type, otherwise the rule call
            // will not be type correct.
            val calledRule = rules(opt.toString)
            if (calledRule.returnType ne null) {
                actions.addBinding(actualReturnType, addExtend(calledRule.returnType))
            }
        }
    }
}

object Modifier extends Enumeration("?", "*", "+") {
    type Val = Value

    val Optional, Star, Plus = Value
}

class NormalRule(pName: String, pTree: List[Any])
    extends NonterminalRule(pName, pTree) {

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

        // Literal call to "foo"
        if ((name eq null) && pattern.startsWith("'")) {
            return
        }

        val varName = if (name eq null) uncapitalize(pattern) else name

        val param = new RParam(varName, pattern, currentBranch, isList)

        checkParamNameConflicts(param)

        params += param
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
                }
            case _ =>
            ()
        }
    }

    def generateClasses(rules: collection.mutable.Map[String, Rule],
            classes: collection.mutable.Map[String, RClass],
            actions: ActionSet) {
        val cl = new RClass(name, "case class")
        classes(name) = cl

        // Rule: if rule returns type that does not match any rule,
        // then create new trait and make rule extend this trait.
        if (returnType ne null) {
            if (!rules.contains(returnType) && !classes.contains(returnType)) {
                classes(returnType) = new RClass(returnType, "trait")
            }
            cl.extend += returnType
        }
    }
}

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

class RClass(val name: String, val classType: String) {
    val extend = new collection.mutable.HashSet[String]
    val params = new ArrayBuffer[RCParam]

    override def toString =
        classType + " " + name + "(" + params.mkString(", ") + ")" +
        (if (extend.isEmpty) "" else " extends " + extend.mkString(" with "))
}

class RCParam(val name: String, val pType: String) {
    override def toString = name + ": " + pType
}

class Gen2(getPos: (Any) => List[Int]) {
    val rules = collection.mutable.Map[String, Rule]()
    val classes = collection.mutable.Map[String, RClass]()
    val actions = new ActionSet
    val ruleRefs = 
            new collection.mutable.HashMap[String, collection.mutable.Set[RParam]]
               with collection.mutable.MultiMap[String, RParam]
            

    def grammargen(tree: Any) {
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                rest.foreach(addRule)
        }

        rules.values.foreach(_.analyze())
        rules.values.foreach(findRuleRefs(_))

        // Do the class generation and type inference.
        for (r <- rules.values) {
            r.generateClasses(rules, classes, actions)

            // Foo returns Bar => case class Foo extends Bar
            if (r.returnType ne null)
                actions.addBinding(r.name, addExtend(r.returnType))
        }

        // Rules are generated, let's run delayed actions
        for ((r, aSet) <- actions) {
            aSet.foreach(_(classes(r)))
        }

        rules.foreach(println)
        println("Rulerefs:\n" + ruleRefs.mkString("\n"))

        println("Classes: \n" + classes.values.mkString("\n"))
    }

    /** Adds rule to symbol table. */
    def addRule(rule: Any) = rule match {
        case "terminal" :: "hidden" :: (name: String) :: rest =>
            rules(name) = new TerminalRule(name, true, rest)
        case "terminal" :: (name: String) :: rest =>
            rules(name) = new TerminalRule(name, false, rest)
        case "fragment" :: (name: String) :: rest =>
            rules(name) = new FragmentRule(name, rest)
        case "option" :: (name: String) :: rest =>
            rules(name) = new OptionRule(name, rest)
        case ":" :: (name: String) :: rest =>
            rules(name) = new NormalRule(name, rest)
        case _ =>
            println("Invalid rule: " + rule)
    }

    def findRuleRefs(rule: Rule) {
        for (p <- rule.params) {
            ruleRefs.addBinding(p.rule, p)
        }
    }
}