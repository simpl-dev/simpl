package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._

abstract class Rule(val name: String, var tree: List[Any]) {
    var returnType: String = null;
    var returnCode: String = null;

    var params = new ArrayBuffer[RParam]

    override def toString = name + " returns " + returnType + " {" + tree +
            "}\nParameters:\n" + params.map(_.toString).mkString("\n")

    def analyze() {
        println("analyze(" + name + ")")
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
                println("matched returns!")
                returnArgs.foreach(matchReturnArg)
                tree = rest
            case _ =>
                ()
        }
    }

    def collectParams(): Unit
}

class FragmentRule(name: String, tree: List[Any])
        extends Rule(name, tree) {
    def collectParams() {}
}

class TerminalRule(name: String, hidden: Boolean, tree: List[Any])
        extends Rule(name, tree) {
    def collectParams() {}
}

abstract class NonterminalRule(name: String, tree: List[Any])
        extends Rule(name, tree) {
}

class OptionRule(name: String, tree: List[Any])
        extends NonterminalRule(name, tree) {
    /** Finds and records all the rule parameters. */
    def collectParams() {
        // TODO
        println("TODO: collect option params: " + tree)
    }
}

object Modifier extends Enumeration("?", "*", "+") {
    type Val = Value

    val Optional, Star, Plus = Value
}

class NormalRule(name: String, tree: List[Any])
    extends NonterminalRule(name, tree) {

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
}

class RParam(val name: String, val rule: String, val branch: BranchIdentifier,
        val isList: Boolean) {
    override def toString = name + ": " + rule +
        (if (isList) ", LIST " else " ") + branch
}

class RClass(val name: String) {
}

class Gen2(getPos: (Any) => List[Int]) {
    val rules = collection.mutable.Map[String, Rule]()
    val classes = collection.mutable.Map[String, RuleClass]()
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
        rules.foreach(println)
        println("Rulerefs:\n" + ruleRefs)
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