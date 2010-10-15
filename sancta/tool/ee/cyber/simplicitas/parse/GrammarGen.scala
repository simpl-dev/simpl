// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class GrammarGen(posMap: Any => List[Int]) {
    object symbols extends SymbolTable {
        val terminals = collection.mutable.Set.empty[String]
        val keywords = new HashMap[String, String]()
        val rules = new HashMap[String, RuleClass]()
    }
    
    import symbols._
    import GrammarUtils._
    
    val error = GrammarUtils.error(posMap)_

    // See the getGrammarSource method for details about how these two
    // code buffers are used and why there two instead of one.

    /** This will contain ANTLR code for terminal rules. */
    private val termCode = new ArrayBuffer[String]()

    /** This will contain ANTLR code for non-terminal rules. */
    private val nonTermCode = new ArrayBuffer[String]()

    /** Name of the grammar. */
    private var grammarName = ""

    /** Java package for grammar */
    private var grammarPackage = ""

    /** First rule in the grammar. This will become the start symbol. */
    private var firstRule: String = null

    /** Grammar-level options that are passed to ANTLR. */
    private var grammarOptions = ""

    /** Checks whether given name can safely be used as identifier, 
      * i.e., it does not clash with Scala keyword. */
    private def checkName(isTerminal: Boolean, typeName: String, name: String, 
            tree: Any) =
        NamingService.validateRuleName(name, typeName, isTerminal) match {
            case Some(errorMsg) => 
                error(tree, errorMsg)
            case _ =>
        }

    /** Parses given rule and adds it to global <code>rules</code> map. */
    def addToSymbolTable(tree: Any) = {
        /** Adds terminal rule to symbol table. */
        def addTerminalDefinition(ruleName: String) {
            // Add the terminal to symbol tables.
            terminals += ruleName
            rules(ruleName) = RuleClass.terminalRule(ruleName)
        }

        def addNonTerminal(name: String, header: String) {
            val ruleClass = new RuleClass(uncapitalize(name) + "_")
            ruleClass.classType = header + " " + name
            rules(name) = ruleClass
        }

        val ruleName = tree match {
            case "terminal" :: "hidden" :: (name: String) :: _ =>
                checkName(true, "Terminal", name, tree)
                addTerminalDefinition(name)
                name
            case "terminal" :: (name: String) :: _ =>
                checkName(true, "Terminal", name, tree)
                addTerminalDefinition(name)
                name
            case "fragment" :: (name: String) :: _ =>
                checkName(true, "Fragment", name, tree)
                // Fragments do not get separate class named after them.
                null
            case "option" :: (name: String) :: _ =>
                checkName(false, "Option", name, tree) 
                addNonTerminal(name, "trait")
                name
            case ":" :: (name: String) :: _ =>
                checkName(false, "Rule", name, tree)
                addNonTerminal(name, "case class")
                name
            case _ => 
                null
        }
        // If this was the first rule then save it.
        if ((firstRule eq null) && (ruleName ne null)) {
            firstRule = ruleName
        }
    }

    def generateRule(tree: Any) {
        val ruleGen = new RuleGen(symbols, termCode, nonTermCode, posMap)
        ruleGen.generate(tree)
    }

    def grammargen(tree: Object) {
        terminals += "EOF"
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                matchGrammarName(nameParts, tree)

                val ruleList = matchGrammarOptions(rest)

                ruleList.foreach(addToSymbolTable)

                ruleList.foreach(generateRule)
        }
//        grammarClass
    }

    /** Parses the full name of the grammar. Fills grammarName and
      * grammarPackage variables. */
    def matchGrammarName(nameParts: List[Any], tree: Object) {
        nameParts.reverse match {
            case (name: String) :: "." :: pname =>
                grammarName = name
                grammarPackage = (pname.reverse foldLeft "")(_+_)
            case List(name: String) => 
                grammarName = name
            case _ => 
                error(tree, "no grammar name")
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

    // Getter methods that are called from Generator.
    
    def getGrammarName = grammarName

    def getTreeSource = grammarClass

    /** Compose the code for the ANTLR grammar. */
    def getGrammarSource = {
        val buffer = new StringBuilder()

        // Generic grammar header.
        buffer.append(grammarHeader)

        // Code for non-terminals.
        nonTermCode.foreach(buffer.append(_))

        // Terminal rules for all the keywords.
        for (kw <- keywords.keys)
            buffer.append(keywords(kw) + ": " + kw + ";\n")

        // Terminal rules for user-defined terminals. NB! these must come
        // last otherwise they shadow the keyword rules.
        termCode.foreach(buffer.append(_))

        buffer.toString
    }

    // Assemble actual source files from various bits and pieces.

    def grammarClass = {
        val treeSrc = new StringBuilder()

        treeSrc append ("package " + grammarPackage + ";\n\n" +
                        "import ee.cyber.simplicitas." +
                          "{CommonNode, CommonToken, TerminalNode}\n" +
                        "import ee.cyber.simplicitas.parse." +
                          "{ErrorHandler}\n\n")

        for (r <- rules.values) {
            treeSrc append r.classType
            if (!r.parameters.isEmpty)
                treeSrc append ("(" + join(
                    r.parameters map (t => t.mod + t.name + ": " + t.vtype)) + ")")
            treeSrc append " extends "
            r.extend.toList match {
                case Nil =>
                    treeSrc append "CommonNode"
                case h :: t =>
                    treeSrc append h
                    if (!t.isEmpty)
                        treeSrc append (" with " + join(t))
            }
            treeSrc append " {"
            if (r.classType startsWith "case ")
                treeSrc append ("\n  def childrenNames = Array(" +
                    join(r.parameters map ('"' + _.name + '"')) + ");\n")
            treeSrc append (r.body + "}\n")
        }
        treeSrc append ("\nobject " + grammarName +
                        "Kind extends Enumeration {\n  type Kind = Value;\n")
        for (t <- terminals)
            treeSrc append ("  val " + t + " = Value(" + grammarName +
                            "Lexer." + t + ");\n")
        treeSrc append
           ("}\n\nclass " + grammarName + "Grammar extends " +
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
                "    parser." + rules(firstRule).antlrName + "()\n" +
                "  }\n" +
            "  def tokenType(token: Int): " + grammarName + "Kind.Kind =\n" +
            "    " + grammarName + "Kind(token);\n" +
            "  def tokenKind(token: Int): Int = token match {\n")
        val reallyKeywords = new ArrayBuffer[String]()
        for (kw <- keywords.keys) {
            val what =
                if (Character isJavaIdentifierPart (kw charAt 1)) {
                    reallyKeywords += kw.substring(1, kw.length - 1)
                    "keyword"
                } else {
                    "operator"
                }
            treeSrc append ("    case " + grammarName + "Lexer." +
                            keywords(kw) + " => " + what + ";\n")
        }
        treeSrc append "    case _ => normal;\n  }\n" +
            "  val keywords: Seq[String] = Array[String](" +
            join(for (kw <- reallyKeywords) yield '"' + kw + '"') +
            ")\n}\n"
            
        treeSrc.toString
    }

    def grammarHeader =
        "grammar " + grammarName + ";\noptions { " +
        "superClass=ParserBase; " + grammarOptions + " }" +
        "\n@header {\npackage " + grammarPackage +
        ";\n import java.util.ArrayList;\n" +
        " import ee.cyber.simplicitas.CommonNode;\n" +
        " import ee.cyber.simplicitas.parse.ParserBase;\n" +
        " import ee.cyber.simplicitas.SourceLocation;\n" +
        " import ee.cyber.simplicitas.parse.TokenLocation;\n}\n" +
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
