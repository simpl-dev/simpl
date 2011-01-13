// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

/**Checks whether some strings can be used as identifiers (rule names,
 * parameter names, etc.). Mainly tries to avoid ANTLR or Scala keywords
 * and very common classes, such as String etc. */
object NamingService {
    def validateRuleName(name: String, typeName: String,
            isTerminal: Boolean): Option[String] =
        if (!isTerminal && !name.charAt(0).isLetter)
            Some(typeName + " name must start with a letter: \"" +
                name + "\"")
        else if (isTerminal && !name.charAt(0).isUpperCase)
            Some(typeName + " name must start with an uppercase letter: \"" +
                name + "\"")
        else if (reservedParserKeywords.contains(name))
            Some(typeName + " name causes an error in generated parser code: \"" +
                name + "\"")
        else if (reservedASTKeywords.contains(name) || scalaKeywords.contains(name))
            Some(typeName + " name causes an error in generated AST code: \"" +
                name + "\"")
        else
            None

    def validateASTAttribute(name : String): Option[String] =
        if (scalaKeywords.contains(name))
            Some("Cannot use attribute name \"" + name + "\" in generated AST code")
        else
            None

    private val reservedParserKeywords = Set(
        "CommonNode", // Simplicitas classes
        "ParserBase",
        "SourceLocation",
        "TokenLocation",
        "AnyVal",        // Scala classes
        "Unit",
        "Boolean",
        "Char",
        "Double",
        "Float",
        "Long",
        "Int",
        "Short",
        "Byte",
        "Seq",
        "List",
        "Option",
        "Null",
        "Nothing",
        "ArrayList", // some Java classes less the Scala classes with the same name
        "Stack",
        "List",
        "Map",
        "HashMap",
        "Class",
        "Integer",
        "Math",
        "Number",
        "Object",
        "Short",
        "String",
        "StringBuffer",
        "System",
        "ANTLRFileStream", // ANTLR org.antlr.runtime.* classes
        "ANTLRInputStream",
        "ANTLRReaderStream",
        "ANTLRStringStream",
        "BaseRecognizer",
        "BitSet",
        "CharStream",
        "CharStreamState",
        "ClassicToken",
        "CommonToken",
        "CommonTokenStream",
        "DFA",
        "EarlyExitException",
        "FailedPredicateException",
        "IntStream",
        "Lexer",
        "MismatchedNotSetException",
        "MismatchedRangeException",
        "MismatchedSetException",
        "MismatchedTokenException",
        "MismatchedTreeNodeException",
        "MissingTokenException",
        "NoViableAltException",
        "Parser",
        "ParserRuleReturnScope",
        "RecognitionException",
        "RecognizerSharedState",
        "RuleReturnScope",
        "SerializedGrammar",
        "Token",
        "TokenRewriteStream",
        "TokenSource",
        "TokenStream",
        "UnwantedTokenException"
    )

    private val reservedASTKeywords = Set(
        "CommonNode", // Simplicitas classes
        "CommonToken",
        "ErrorHandler",
        "TerminalNode"
    )

    private val scalaKeywords = Set(
        "abstract",
        "case",
        "catch",
        "class",
        "def",
        "do",
        "else",
        "extends",
        "false",
        "final",
        "finally",
        "for",
        "forSome",
        "if",
        "implicit",
        "import",
        "lazy",
        "match",
        "new",
        "null",
        "object",
        "override",
        "package",
        "private",
        "protected",
        "requires",
        "return",
        "sealed",
        "super",
        "this",
        "throw",
        "trait",
        "try",
        "true",
        "type",
        "val",
        "var",
        "while",
        "with",
        "yield"
    )
}
