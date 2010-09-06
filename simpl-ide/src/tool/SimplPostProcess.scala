// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.ide

import ee.cyber.simplicitas._
import ee.cyber.simplicitas.parse.NamingService

class SimplPostProcess(val ctx: SimplCtx) {
  def validate() {
    // Resolve links in program.
    ctx.tree.walkTree(resolve)
  }

  def uncapitalize(s: String): String =
    if (s == "") ""
    else (Character toLowerCase (s charAt 0)) + (s substring 1)
  
  def resolve(node: CommonNode) {
    node match {
      case ref: RuleRef =>
        if (ctx.nonTerms.contains(ref.id.text)) {
          ref.ref = ctx.nonTerms(ref.id.text)
        } else if (ctx.options.contains(ref.id.text)) {
          ref.ref = ctx.options(ref.id.text)
        } else if (ctx.terminals.contains(ref.id.text)) {
          ref.ref = ctx.terminals(ref.id.text)
        } else {
          addError("Undefined rule", ref.id)
        } 
        
        if ((ref.ref ne null) && 
                ref.parent.isInstanceOf[MatchToken] && 
                (ref.parent.parent.asInstanceOf[Match].name eq null))
            NamingService.validateASTAttribute(uncapitalize(ref.id.text)) match {
                case Some(errorMessage) => addSimpleError(errorMessage, ref.id)
                case _ =>
            }
      case ref: FragmentRef =>
        if (ctx.fragments.contains(ref.id.text)) {
          ref.ref = ctx.fragments(ref.id.text)
        } else if (ctx.terminals.contains(ref.id.text)) {
          ref.ref = ctx.terminals(ref.id.text)
        } else {
          addError("Undefined fragment or terminal", ref.id)
        }
      case tDef: TerminalDef => validate(tDef, "Terminal", true)
      case fDef: FragmentDef => validate(fDef, "Fragment", true)
      case oDef: OptionDef => validate(oDef, "Option", false)
      case rDef: RuleDef => validate(rDef, "Rule", false)
      case m: Match if (m.name ne null) => 
        NamingService.validateASTAttribute(m.name.text) match {
            case Some(errorMsg) => addSimpleError(errorMsg, m.name)
            case _ =>
        }
        if ((m.token ne null) && (m.token.alt ne null))
            addSimpleError("The following pattern cannot be given a name", m.name)
      case _ => ()
    }
  }

  private def validate(rDef: RuleDef, typeName: String, isTerminal: Boolean) =
    NamingService.validateRuleName(rDef.name.text, typeName, isTerminal) match {
        case Some(errorMsg) => addSimpleError(errorMsg, rDef.name)
        case _ =>
    }

  private def addError(errorPrefix: String, id: Id) =
    addSimpleError(errorPrefix + ": \"" + id.text + "\"", id)

  private def addSimpleError(errorPrefix: String, id: Id) =
    ctx.errors += new SourceMessage(errorPrefix, SourceMessage.Error, id)
}
