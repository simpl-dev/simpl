package ee.cyber.simplicitas.parse

abstract class Rule(val name: String, var tree: List[Any]) {
    var returnType: String = null;
    var returnCode: String = null;

    override def toString = name + " returns " + returnType + " {" + tree + "}"
}

class FragmentRule(name: String, tree: List[Any])
    extends Rule(name, tree) {
}

class TerminalRule(name: String, hidden: Boolean, tree: List[Any])
    extends Rule(name, tree) {
}

abstract class NonterminalRule(name: String, tree: List[Any])
    extends Rule(name, tree) {
}

class OptionRule(name: String, tree: List[Any])
    extends NonterminalRule(name, tree) {
}

class NormalRule(name: String, tree: List[Any])
    extends NonterminalRule(name, tree) {
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

        rules.values.foreach(analyze)
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

    def analyze(rule: Rule) {
        println("analyze(" + rule.name + ")")
        matchReturns(rule)
    }

    def matchReturns(rule: Rule) {
        def matchReturnArg(arg: Any) {
            arg match {
                case rt: String =>
                    rule.returnType = rt
                case List("BODY", body) =>
                    rule.returnCode = body.toString
                case _ =>
                    println("Invalid return arg: " + arg)
            }
        }

        rule.tree match {
            case ("returns" :: returnArgs) :: rest =>
                println("matched returns!")
                returnArgs.foreach(matchReturnArg)
                rule.tree = rest
            case _ =>
                ()
        }
    }
}