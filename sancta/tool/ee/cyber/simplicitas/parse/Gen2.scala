package ee.cyber.simplicitas.parse

abstract class Rule(val name: String, var tree: List[Any]) {
    var returnType: String = null;
    var returnCode: String = null;

    var params: Seq[RParam] = Nil

    override def toString = name + " returns " + returnType + " {" + tree + "}"

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
        ruleCall match {
            // Foo
            case rule: String =>
                doRuleCall(null, rule)
            // foo=Bar
            case List("=", name: String, rule: String) =>
                doRuleCall(name, rule)
            // (foo)
            case "(" :: options => 
                val oldList = isList
                val oldBranch = currentBranch

                currentBranch = currentBranch.extend
                doOptionList(options)

                currentBranch = oldBranch
                isList = oldList
        }
    }

    def doRuleCall(name: String, rule: String) {
        println("doRuleCall(" + name + ", " + rule + "), branch=" +
                currentBranch + ", list=" + isList)
    }
}

class RParam() {
}

class RClass(val name: String) {
}

class Gen2(getPos: (Any) => List[Int]) {
    val rules = scala.collection.mutable.Map[String, Rule]()
    val classes = scala.collection.mutable.Map[String, RuleClass]()

    def grammargen(tree: Any) {
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                rest.foreach(addRule)
        }

        rules.values.foreach(_.analyze())
        rules.foreach(println)
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
}