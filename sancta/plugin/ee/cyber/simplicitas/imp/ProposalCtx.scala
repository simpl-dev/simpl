// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import ee.cyber.simplicitas._
import org.eclipse.imp.editor.SourceProposal
import org.eclipse.jface.text.Region


/** Context for generating proposals for auto-complete feature. */
trait ProposalCtx {
    /** Node which currently lies under the cursor. May be null, if the DSL
      * program is empty or contains parsing error. */
    def node: CommonNode

    /** Parent of the node currently under cursor, or null. */
    def parent: CommonNode = {
        val n = node
        if (n eq null)
            null
        else
            node.parent
    }

    /** Token that is currently under cursor. */
    def token: GenericToken
    
    /** Text preceding the cursor. For example, if the current token is
      * abc|def ("|" marks the cursor), then prefix will be "abc". */
    def prefix: String
    
    /** Cursor position, character offset from beginning of the file. */
    def offset: Int
    
    /** Access to the grammar of this language. */
    def grammar: GenericGrammar

    /** Adds new proposal to list of completion proposals. */
    def add(proposal: SourceProposal)

    /** Adds new propsal.
      * @param propsal Text that will be shown in the popup window.
      * @param newText Text that will actually be inserted into program. */
    def add(proposal: String, newText: String) {
        add(new SourceProposal(proposal, newText, prefix, offset))
    }
    
    /** Adds new propsal.
      * @param propsal Text that will be shown in the popup window.
      * @param newText Text that will actually be inserted into program.
      * @param documentation Text will be displayed as documentation with
      *        the proposal. */
    def add(proposal: String, newText: String, documentation: String) {
        add(new SourceProposal(proposal, newText, prefix,
                               new Region(offset, 0), documentation))
    }

    /** Adds new propsal.
      * @param propsal Text that will inserted into program. */
    def add(newText: String) {
        add(new SourceProposal(newText, prefix, offset))
    }

    private def matchingKeywords: Seq[String] =
        grammar.keywords filter {kw => (kw startsWith prefix) && kw != prefix}

    /** Add proposals corresponding to keywords in the language. */
    def addKeywords() {
        matchingKeywords foreach add
    }
}
