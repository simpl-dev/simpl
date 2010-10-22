package ee.cyber.simplicitas.parse

abstract class Rule(name: String, tree: List[Any]) {
    var returnType: String = null;

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

class Gen2(getPos: (Any) => List[Int]) {
    val rules = scala.collection.mutable.Map[String, Rule]()

    def grammargen(tree: Any) {
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                rest.foreach(addRule)
        }

        rules.foreach(println)
        rules.values.foreach(analyze)
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
    }
}