// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp.parser

import ee.cyber.simplicitas.imp.{ParseCtx, APluginConfig}

import org.eclipse.imp.model.ISourceProject
import org.eclipse.imp.parser.{IParseController,
                               SimpleAnnotationTypeInfo,
                               IMessageHandler}
import org.eclipse.imp.services.ILanguageSyntaxProperties
import org.eclipse.imp.language.Language

import org.eclipse.core.runtime.{IProgressMonitor, IPath}
import org.eclipse.jface.text.IRegion

import ee.cyber.simplicitas._

import java.util.{SortedMap, Iterator, TreeMap, ArrayList, Collections}

class SimplicitasParseController(language: Language,
                                 config: APluginConfig)
        extends IParseController {
  lazy final val getAnnotationTypeInfo = new SimpleAnnotationTypeInfo()
  lazy val getSyntaxProperties: ILanguageSyntaxProperties =
    new ILanguageSyntaxProperties() {
      def getSingleLineCommentPrefix: String = config.singleLineCommentPrefix
      def getBlockCommentStart: String = null
      def getBlockCommentContinuation: String = null
      def getBlockCommentEnd: String = null
      def getFences: Array[Array[String]] =
        for ((x, y) <- config.fences)
          yield Array(x, y)
      def getIdentifierConstituentChars: String = null
      def getIdentifierComponents(x: String): Array[Int] = null
    }
  lazy final val getSourcePositionLocator =
    new SimplicitasSourcePositionLocator(this)
  def getLanguage = language
  var path: IPath = null
  var project: ISourceProject = null
  var handler: IMessageHandler = null
  var tokens: SortedMap[Int, GenericToken] = null

  var grammar: GenericGrammar = null

  def initialize(path: IPath,
                 project: ISourceProject,
                 handler: IMessageHandler) {
    this.path = path
    this.project = project
    this.handler = handler
    this.tokens = null
    this.grammar = null
    println("INITIALIZE CALLED, handler = " + handler)
  }

  def getPath = path
  def getProject = project

  def getTokenIterator(region: IRegion): Iterator[GenericToken] = {
    val m = tokens.headMap(region.getOffset + 1)
    if (m.isEmpty) {
        println("getTokenIterator: no tokens available")
        return Collections.emptySet().iterator()
    }
    new ArrayList[GenericToken](
      tokens.subMap(m.lastKey(),
                    region.getOffset + region.getLength).values).iterator
  }

  def getCurrentAst =
    if (grammar hasTree)
        grammar.tree
    else
        null

  def getTokenByIndex(i: Int): GenericToken = grammar.tokens(i);

  def parse(input: String, monitor: IProgressMonitor): Object = {
    val ctx = new ParseCtx() {
        def parse(g: GenericGrammar): Boolean = {
            g.parseString(input)
            for (tok <- g.tokens) {
                tokens.put(tok.startIndex, tok)
            }
            grammar = g
            reportErrors(g.errors)
            g.errors.isEmpty
        }

        def reportErrors(errors: Iterable[SourceMessage]) {
            if (handler eq null) {
                println("WARNING: NULL MESSAGE HANDLER!")
                return
            }
            for (err <- errors) {
                if (err ne null) {
                    handler.handleSimpleMessage(err.message,
                            err.startOffset, err.endOffset, 0, 0, 0, 0)
                }
            }
        }
    }
    tokens = Collections.synchronizedSortedMap(
      new TreeMap[Int, GenericToken]())
    config.parse(ctx)
    grammar.tree
  }

  def getTokenAt(offset: Int): GenericToken = {
    val head = tokens.headMap(offset + 1)

    if (head.isEmpty) {
      // empty file
      return null
    }
    val token = head.get(head.lastKey)
    if (token.startIndex <= offset && token.endIndex >= offset)
      token
    else
      null
  }
}
