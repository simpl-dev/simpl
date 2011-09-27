// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import org.eclipse.imp.parser.IParseController
import org.eclipse.imp.services.IContentProposer
import org.eclipse.imp.editor.SourceProposal
import org.eclipse.jface.text.{ITextViewer, IRegion}
import org.eclipse.jface.text.contentassist.ICompletionProposal

import ee.cyber.simplicitas._
import ee.cyber.simplicitas.imp.parser.SimplicitasParseController

import scala.collection.mutable.ArrayBuffer

class SimplicitasContentProposer(config: APluginConfig)
    extends IContentProposer {
  def getContentProposals(ctlr: IParseController,
                          offsetx: Int,
                          viewer: ITextViewer): Array[ICompletionProposal] = {
    println("offset: " + offsetx)

    val myCtlr = ctlr.asInstanceOf[SimplicitasParseController]
    val tokenx = getToken(myCtlr, offsetx)
    println("Token: \"" + tokenx + "\"")
    if (tokenx eq null) {
      return new Array[ICompletionProposal](0)
    }
    val prefixStr =
      if (canBeCompleted(ctlr, tokenx))
        tokenx.text.substring(0, offsetx - tokenx.startIndex)
      else
        ""

    println("Token: \"" + tokenx + "\", prefix: \"" + prefixStr + "\"")
    val astNode = ctlr.getSourcePositionLocator.findNode(
        ctlr.getCurrentAst,
        tokenx.startIndex,
        tokenx.endIndex)

    if (astNode eq null)
        println("No node...")

    val proposals = new ArrayBuffer[SourceProposal]()
    val ctx = new ProposalCtx() {
        def node = astNode.asInstanceOf[CommonNode]
        def token = tokenx
        def prefix = prefixStr
        def offset = offsetx
        def grammar = myCtlr.grammar
        def add(proposal: SourceProposal) {
            proposals += proposal
        }
    }

    config propose ctx
    val propArray = proposals.toArray
    util.Sorting.stableSort[SourceProposal, String](propArray,
                                                    _.getDisplayString)
    return propArray.toArray
  }

  def canBeCompleted(ctlr: IParseController, token: GenericToken): Boolean =
    config canBeCompleted token

  class MyRegion(offset: Int) extends IRegion {
    def getLength(): Int = 0
    def getOffset(): Int = offset
  }

  private def getToken(ctlr: SimplicitasParseController,
                       offset: Int): GenericToken = {
    def atEndOfWord(token: GenericToken): Boolean =
      token.startIndex == offset

    val token = ctlr.getTokenAt(offset)
    if ((token eq null) || atEndOfWord(token)) {
      // Check, whether previous token is better
      val prevToken = ctlr.getTokenAt(offset - 1)

      if ((token eq null) ||
              token.isHidden ||
              canBeCompleted(ctlr, prevToken)) {
        return prevToken
      }
    }

    return token
  }
}
