package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._
import Actions._

trait STable {
    def rules: collection.mutable.Map[String, Rule]
    def classes: collection.mutable.Map[String, RClass]
    def actions: ActionSet

    /** Map from kw contents to name of rule. */
    def keywords: collection.mutable.Map[String, String]

    def newId: String
}

class Gen2(getPos: (Any) => List[Int]) {
    object Symbols extends STable {
        val rules = collection.mutable.Map[String, Rule]()
        val classes = collection.mutable.Map[String, RClass]()
        val actions = new ActionSet
        val keywords = collection.mutable.Map[String, String]()

        private var idVal = 0

        def newId = {idVal += 1; "Z" + idVal}
    }

    import Symbols._
    
    /** Name of the grammar. */
    var grammarName: String = null

    /** Java package for grammar */
    var grammarPackage: String = null

    /** Grammar-level options that are passed to ANTLR. */
    private var grammarOptions = ""

    val error = GrammarUtils.error(getPos)_

    def grammargen(tree: Any) {
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                matchGrammarName(nameParts, tree)
                val ruleList = matchGrammarOptions(rest)
                ruleList.foreach(addRule)
        }

        // toSet is necessary because in the analysis step some additional
        // rules can be generated.
        rules.values.toSet[Rule].foreach(_.analyze())

        // Do the class generation and type inference.
        for (r: Rule <- rules.values.toSet) {
            println("generateClasses(" + r.name + ")")
            r.generateClasses()
        }

        // Rules are generated, let's run delayed actions
        for ((r, aSet) <- actions) {
            aSet.foreach(_(classes(r)))
        }

        cleanupExtends()
    }

    /** Adds rule to symbol table. */
    def addRule(rule: Any) = rule match {
        case "terminal" :: "hidden" :: (name: String) :: rest =>
            rules(name) = new TerminalRule(name, true, rest, Symbols)
        case "terminal" :: (name: String) :: rest =>
            rules(name) = new TerminalRule(name, false, rest, Symbols)
        case "fragment" :: (name: String) :: rest =>
            rules(name) = new FragmentRule(name, rest, Symbols)
        case "option" :: (name: String) :: rest =>
            rules(name) = new OptionRule(name, rest, Symbols)
        case ":" :: (name: String) :: rest =>
            rules(name) = new NormalRule(name, rest, Symbols)
        case _ =>
            error(rule, "Malformed rule")
    }

    /** Cleans up the extends declarations:
      * Detect cycles
      * Remove "A extends A"
      * If "A extends B with C" and "B extends C" => "A extends B"
      */
    def cleanupExtends() {
        // TODO: detect cycles.

        def reachableSet(items: collection.mutable.Set[String]):
                collection.mutable.Set[String] = {
            val newItems = items.flatMap(
                    (name: String) =>
                        if (classes.contains(name))
                            classes(name).extend
                        else
                            Nil)

            if (newItems.forall(items.contains(_)))  // No new classes
                items
            else
                reachableSet(items ++ newItems)
        }

        for (cl <- classes.values) {
            cl.extend -= cl.name

            for (ext <- cl.extend.toList) {
                if (reachableSet(cl.extend - ext).contains(ext)) {
                    cl.extend -= ext
                }
            }
        }
    }

    /** Parses the full name of the grammar. Fills grammarName and
      * grammarPackage variables. */
    def matchGrammarName(nameParts: List[Any], tree: Any) {
        nameParts.reverse match {
            case (name: String) :: "." :: pname =>
                grammarName = name
                grammarPackage = (pname.reverse foldLeft "")(_+_)
            case List(name: String) => 
                grammarName = name
            case _ =>
                error(tree, "No grammar name")
        }
    }

    /** Parses grammar-level "options(foo = bar;)" declaration. */
    def matchGrammarOptions(tree: Any) = tree match {
        case ("options" :: opts) :: rules =>
            for (List(name, value) <- opts) {
                grammarOptions += " " + name + "=" + value + ";"
            }
            rules
        case rules: List[Any] => 
            rules
    }

    def getTreeSource = {
        val ret = new StringBuilder()

        ret.append(treeHeader)

        for (c <- classes.values) {
            c.generate(ret)
        }

        ret.toString
    }

    def treeHeader =
        "package " + grammarPackage + ";\n\n" +
        "import ee.cyber.simplicitas." +
            "{CommonNode, CommonToken, TerminalNode, LiteralNode}\n" +
        "import ee.cyber.simplicitas.parse." +
            "{ErrorHandler}\n\n"

    def getGrammarSource = {
        val ret = new StringBuilder()

        ret.append(grammarHeader)

        for (r <- rules.values if r.isInstanceOf[NonterminalRule]) {
            r.generateGrammar(ret)
        }

        // Artificially generated keyword rules precede the terminal rules.
        // Otherwise they can be shadowed by the regular expressions in
        // the terminal rules.
        for (r <- rules.values if r.isInstanceOf[LiteralRule]) {
            r.generateGrammar(ret)
        }

        for (r <- rules.values if 
                r.isInstanceOf[TerminalRule] || r.isInstanceOf[FragmentRule]) {
            r.generateGrammar(ret)
            ret.append("\n")
        }

        ret.toString
    }

    def grammarHeader =
        "grammar " + grammarName + ";\n" +
        "options {\n" +
            "superClass=ParserBase;\n" +
            grammarOptions + "\n" +
		"}\n\n" +
        "@header {\n" +
            "package " + grammarPackage + ";\n" +
            "import java.util.ArrayList;\n" +
            "import ee.cyber.simplicitas.CommonNode;\n" +
            "import ee.cyber.simplicitas.LiteralNode;\n" +
            "import ee.cyber.simplicitas.parse.ParserBase;\n" +
            "import ee.cyber.simplicitas.SourceLocation;\n" +
            "import ee.cyber.simplicitas.parse.TokenLocation;\n" +
        "}\n" +
        "@lexer::header { package " + grammarPackage + """; }
@lexer::members {
    private ee.cyber.simplicitas.parse.ErrorHandler handler = null;
    public void reportError(RecognitionException ex) {
        handler.reportError(ex);
        super.reportError(ex);
    }

    public void emitErrorMessage(String s) {
        handler.emitErrorMessage(s);
    }

    public void setErrorHandler(ee.cyber.simplicitas.parse.ErrorHandler handler) {
        this.handler = handler;
    }

    public void displayRecognitionError(String[] tokenNames,
            RecognitionException e) {
        emitErrorMessage(getErrorMessage(e, tokenNames));
    }
}
    """
}