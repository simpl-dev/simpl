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

    /** Returns position of the given node.
      * Strictly does not belong to symbol table, but it is convenient to
      * pass it around along with other global-ish information. */
    def getPos: (Any) => List[Int]

    def newId: String
}

class Gen2(pGetPos: (Any) => List[Int]) {
    object Symbols extends STable {
        val rules = collection.mutable.Map[String, Rule]()
        val classes = collection.mutable.Map[String, RClass]()
        val actions = new ActionSet
        val keywords = collection.mutable.Map[String, String]()

        private var idVal = 0

        def getPos = pGetPos

        def newId = {idVal += 1; "Z" + idVal}
    }

    import Symbols._
    
    /** Name of the grammar. */
    var grammarName: String = null

    /** Java package for grammar */
    var grammarPackage: String = null

    /** Grammar-level options that are passed to ANTLR. */
    private var grammarOptions = ""

    private var firstRule: String = null

    val error = GrammarUtils.error(pGetPos)_

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
            if (firstRule == null) {
                firstRule = name
            }
        case ":" :: (name: String) :: rest =>
            rules(name) = new NormalRule(name, rest, Symbols)
            if (firstRule == null) {
                firstRule = name
            }
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
        
        tokenKind(ret)
        grammarClass(ret)
        

        ret.toString
    }

    def treeHeader =
        "package " + grammarPackage + ";\n\n" +
        "import ee.cyber.simplicitas." +
            "{CommonNode, CommonToken, TerminalNode, LiteralNode}\n" +
        "import ee.cyber.simplicitas.parse." +
            "{ErrorHandler}\n\n"

    def tokenKind(buf: StringBuilder) {
        buf.append("\nobject " + grammarName +
                        "Kind extends Enumeration {\n  type Kind = Value;\n")
        for (r <- rules.values if r.isInstanceOf[TerminalRule] ||
                r.isInstanceOf[LiteralRule]) {
            buf.append("    val " + r.name + " = Value(" + grammarName +
                            "Lexer." + r.name + ");\n")
        }
        buf.append("}\n")
    }

    def grammarClass(buf: StringBuilder) {
        buf.append(
            "\nclass " + grammarName + "Grammar extends " +
                "ee.cyber.simplicitas.parse.GrammarBase[" +
                firstRule + ", " + grammarName + "Kind.Kind] {\n" +
            "  type Token = CommonToken[" + grammarName + "Kind.Kind]\n" +
            "  def lexer(source: org.antlr.runtime.CharStream, errorHandler:" +
                " ErrorHandler): org.antlr.runtime.TokenSource = {\n" +
                "    val lex = new " + grammarName + "Lexer(source)\n" +
                "    lex.setErrorHandler(errorHandler)\n" +
                "    lex\n  }\n" +
            "  def doParse(tokens: org.antlr.runtime.TokenStream," +
                " errorHandler: ErrorHandler): " + firstRule + " = {\n" +
                "    val parser = new " + grammarName + "Parser(tokens)\n" +
                "    parser.errorHandler = errorHandler\n" +
                "    parser.toplevel()\n" +
                "  }\n" +
            "  def tokenType(token: Int): " + grammarName + "Kind.Kind =\n" +
            "    " + grammarName + "Kind(token);\n" +
            "  def tokenKind(token: Int): Int = token match {\n")
        val reallyKeywords = new ArrayBuffer[String]()
        for (kw <- keywords.keys) {
            val what =
                if (Character.isJavaIdentifierPart(kw charAt 1)) {
                    reallyKeywords += kw.substring(1, kw.length - 1)
                    "keyword"
                } else {
                    "operator"
                }
            buf.append("    case " + grammarName + "Lexer." +
                            keywords(kw) + " => " + what + ";\n")
        }
        buf.append("    case _ => normal;\n  }\n" +
            "  val keywords: Seq[String] = Array[String](" +
            join(for (kw <- reallyKeywords) yield '"' + kw + '"') +
            ")\n}\n")
    }

    def getGrammarSource = {
        val ret = new StringBuilder()

        ret.append(grammarHeader)

        ret.append(getFirstRule)

        for (r <- rules.values if !r.isTerminalRule) {
            r.generateGrammar(ret)
            ret.append("\n")
        }

        // Artificially generated keyword rules precede the terminal rules.
        // Otherwise they can be shadowed by the regular expressions in
        // the terminal rules.
        for (r <- rules.values if r.isInstanceOf[LiteralRule]) {
            r.generateGrammar(ret)
            ret.append("\n")
        }

        for (r <- rules.values if 
                r.isInstanceOf[TerminalRule] || r.isInstanceOf[FragmentRule]) {
            r.generateGrammar(ret)
            ret.append("\n")
        }

        ret.toString
    }

    private def getFirstRule = {
        val tlRule = rules(firstRule)

        "toplevel returns [" + tlRule.actualReturnType + " r]: " +
                "tl=" + tlRule.antlrName + "{$r = $tl.r;};\n\n"
    }

    private def grammarHeader =
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
