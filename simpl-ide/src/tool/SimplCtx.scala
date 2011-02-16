// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.ide;

import ee.cyber.simplicitas._

class SimplCtx(val grammar: SimplGrammar) {
  import collection.mutable.{Map, ArrayBuffer}

  val tree = grammar.tree

  val nonTerms = Map[String, RuleDef]()
  val options = Map[String, RuleDef]()
  val terminals = Map[String, RuleDef]()
  val fragments = Map[String, RuleDef]()

  var errors = new ArrayBuffer[SourceMessage]()

  makeSymbolTable()

  readDocComments()

  def makeSymbolTable() {
    tree.rules foreach makeSymbolTable
  }

  def makeSymbolTable(r: RuleDef) {
    r match {
      case nd: NonTerminalDef => nonTerms += (nd.name.text -> nd)
      case o: OptionDef => options += (o.name.text -> o)
      case t: TerminalDef => terminals += (t.name.text -> t)
      case f: FragmentDef => fragments += (f.name.text -> f)
    }
  }

  def getDocContent(str: String) =
    str.substring(3, str.length - 2).trim

  def readDocComments() {
    val docComments = getComments
    for (rule <- tree.rules) {
      if (docComments.contains(rule.startIndex)) {
        rule.documentation = getDocContent(docComments(rule.startIndex))
      }
    }
  }

  def isDocComment(token: SimplGrammar#Token) =
    token.kind == SimplKind.MlComment &&
        token.text.length > 4 &&
        token.text.startsWith("/**")

  def getComments = {
    val ret = Map[Int, String]()

    var comment: String = null
    for (tok <- grammar.tokens) {
      if (isDocComment(tok)) {
        comment = tok.text
      } else if ((comment ne null) && !tok.isHidden) {
        ret += (tok.startIndex -> comment)
        comment = null
      }
    }

    ret
  }
}
